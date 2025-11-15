import { describe, it, expect } from "vitest"
import { Effect } from "effect"
import { WASMService, WASMServiceLive } from "./wasm"
import type { Inputs } from "$lib/wasm/types"

describe("WASMService - Validation", () => {
  const validInputs: Inputs = {
    spot: 100,
    strike: 100,
    timeToExpiry: { days: 30 },
    volatility: 0.2,
    riskFreeRate: 0.05,
    kind: "Call",
  }

  // Note: Tests that load the actual WASM module are skipped in unit tests
  // Integration tests with the real WASM module should be run in a browser environment
  // These tests verify the validation layer works correctly

  it("rejects negative spot price", async () => {
    const invalidInputs: Inputs = {
      ...validInputs,
      spot: -100,
    }

    const program = Effect.gen(function* () {
      const wasm = yield* WASMService
      return yield* wasm.calculatePrice(invalidInputs)
    })

    await expect(
      Effect.runPromise(program.pipe(Effect.provide(WASMServiceLive))),
    ).rejects.toThrow()
  })

  it("rejects negative strike price", async () => {
    const invalidInputs: Inputs = {
      ...validInputs,
      strike: -100,
    }

    const program = Effect.gen(function* () {
      const wasm = yield* WASMService
      return yield* wasm.calculatePrice(invalidInputs)
    })

    await expect(
      Effect.runPromise(program.pipe(Effect.provide(WASMServiceLive))),
    ).rejects.toThrow()
  })

  it("rejects negative time to expiry", async () => {
    const invalidInputs: Inputs = {
      ...validInputs,
      timeToExpiry: { days: -30 },
    }

    const program = Effect.gen(function* () {
      const wasm = yield* WASMService
      return yield* wasm.calculatePrice(invalidInputs)
    })

    await expect(
      Effect.runPromise(program.pipe(Effect.provide(WASMServiceLive))),
    ).rejects.toThrow()
  })

  it("rejects negative volatility", async () => {
    const invalidInputs: Inputs = {
      ...validInputs,
      volatility: -0.2,
    }

    const program = Effect.gen(function* () {
      const wasm = yield* WASMService
      return yield* wasm.calculatePrice(invalidInputs)
    })

    await expect(
      Effect.runPromise(program.pipe(Effect.provide(WASMServiceLive))),
    ).rejects.toThrow()
  })

  it("rejects negative risk-free rate", async () => {
    const invalidInputs: Inputs = {
      ...validInputs,
      riskFreeRate: -0.05,
    }

    const program = Effect.gen(function* () {
      const wasm = yield* WASMService
      return yield* wasm.calculatePrice(invalidInputs)
    })

    await expect(
      Effect.runPromise(program.pipe(Effect.provide(WASMServiceLive))),
    ).rejects.toThrow()
  })
})
