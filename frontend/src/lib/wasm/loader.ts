import { Effect } from "effect"
import { WASMLoadError } from "$lib/errors/wasm"
import type { Inputs, OptionPrice } from "./types"

export interface WASMInstance {
  readonly calculateBlackScholes: (input: Inputs) => OptionPrice
  readonly addNumbers: (x: number, y: number) => number
  readonly doubleValue: (n: number) => number
  readonly helloWasm: () => void
}

interface WASMExports {
  readonly memory: WebAssembly.Memory
  readonly calculateBlackScholes: (input: string) => string
  readonly addNumbers: (x: number, y: number) => number
  readonly doubleValue: (n: number) => number
  readonly helloWasm: () => void
  readonly rts_schedulerLoop: () => void
  readonly rts_freeStablePtr: (ptr: number) => void
  readonly rts_promiseThrowTo: (ptr: number, err: unknown) => void
  readonly rts_promiseResolveUnit: (ptr: number) => void
  readonly rts_promiseReject: (ptr: number, err: unknown) => void
}

type JsffiInit = (exports: WASMExports) => WebAssembly.ModuleImports

let cachedInstance: WASMInstance | null = null

export const loadWASM = (): Effect.Effect<WASMInstance, WASMLoadError> =>
  Effect.gen(function* () {
    if (cachedInstance) {
      return cachedInstance
    }

    const wasmModule = yield* Effect.tryPromise({
      try: () => fetch("/wasm/dist/quanty.wasm"),
      catch: cause => new WASMLoadError({ cause }),
    })

    const wasmBytes = yield* Effect.tryPromise({
      try: () => wasmModule.arrayBuffer(),
      catch: cause => new WASMLoadError({ cause }),
    })

    const jsffiModule = yield* Effect.tryPromise({
      try: () =>
        import("/wasm/dist/ghc_wasm_jsffi.js") as Promise<{
          default: JsffiInit
        }>,
      catch: cause => new WASMLoadError({ cause }),
    })

    const jsffiInit = jsffiModule.default

    const compiled = yield* Effect.tryPromise({
      try: () => WebAssembly.compile(wasmBytes),
      catch: cause => new WASMLoadError({ cause }),
    })

    const { instance } = yield* Effect.tryPromise({
      try: async () => {
        const inst = await WebAssembly.instantiate(compiled, {
          ghc_wasm_jsffi: {} as WebAssembly.ModuleImports,
        })
        const exports = inst.exports as unknown as WASMExports
        const imports = jsffiInit(exports)
        return WebAssembly.instantiate(compiled, {
          ghc_wasm_jsffi: imports,
        })
      },
      catch: cause => new WASMLoadError({ cause }),
    })

    const exports = instance.exports as unknown as WASMExports

    const wrappedInstance: WASMInstance = {
      calculateBlackScholes: (input: Inputs): OptionPrice => {
        const inputJson = JSON.stringify(input)
        const resultJson = exports.calculateBlackScholes(inputJson)
        const result = JSON.parse(resultJson) as OptionPrice | { error: string }

        if ("error" in result) {
          throw new Error(result.error)
        }

        return result
      },
      addNumbers: (x: number, y: number) => exports.addNumbers(x, y),
      doubleValue: (n: number) => exports.doubleValue(n),
      helloWasm: () => exports.helloWasm(),
    }

    cachedInstance = wrappedInstance
    return wrappedInstance
  })
