import { Effect } from "effect"
import { WASMLoadError } from "$lib/errors/wasm"
import type { Inputs, OptionPrice } from "./types"
import { WASI } from "@bjorn3/browser_wasi_shim"

export interface WASMInstance {
  readonly calculateBlackScholes: (input: Inputs) => OptionPrice
  readonly addNumbers: (x: number, y: number) => number
  readonly doubleValue: (n: number) => number
  readonly helloWasm: () => void
}

interface WASMExports {
  readonly memory: WebAssembly.Memory
  readonly hs_init: () => Promise<void>
  readonly calculateBlackScholes: (input: string) => string
  readonly addNumbers: (x: number, y: number) => number
  readonly doubleValue: (n: number) => number
  readonly helloWasm: () => void
}

type JsffiInit = (exports: WASMExports) => WebAssembly.ModuleImports

let cachedInstance: WASMInstance | null = null

export const loadWASM = (): Effect.Effect<WASMInstance, WASMLoadError> =>
  Effect.gen(function* () {
    if (cachedInstance) {
      return cachedInstance
    }

    const jsffiModule = yield* Effect.tryPromise({
      try: () =>
        import("$lib/wasm/ghc_wasm_jsffi.js") as Promise<{
          default: JsffiInit
        }>,
      catch: cause => new WASMLoadError({ cause }),
    })

    const jsffiInit = jsffiModule.default

    const wasi = new WASI([], [], [])

    const instance = yield* Effect.tryPromise({
      try: async () => {
        const response = await fetch("/wasm/dist/quanty.wasm")
        if (!response.ok) {
          throw new Error(`Failed to fetch WASM: ${response.statusText}`)
        }

        const instanceExports: Record<string, unknown> = {}

        const wasmInstance = await WebAssembly.instantiateStreaming(response, {
          wasi_snapshot_preview1: wasi.wasiImport,
          ghc_wasm_jsffi: jsffiInit(instanceExports as WASMExports),
        })

        Object.assign(instanceExports, wasmInstance.instance.exports)

        wasi.initialize(wasmInstance.instance)

        const exports = instanceExports as unknown as WASMExports
        await exports.hs_init()

        return exports
      },
      catch: cause => new WASMLoadError({ cause }),
    })

    const wrappedInstance: WASMInstance = {
      calculateBlackScholes: (input: Inputs): OptionPrice => {
        const inputJson = JSON.stringify(input)
        const resultJson = instance.calculateBlackScholes(inputJson)
        const result = JSON.parse(resultJson) as OptionPrice | { error: string }

        if ("error" in result) {
          throw new Error(result.error)
        }

        return result
      },
      addNumbers: (x: number, y: number) => instance.addNumbers(x, y),
      doubleValue: (n: number) => instance.doubleValue(n),
      helloWasm: () => instance.helloWasm(),
    }

    cachedInstance = wrappedInstance
    return wrappedInstance
  })
