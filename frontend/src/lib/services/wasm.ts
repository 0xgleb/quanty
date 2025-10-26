import { Context, Effect, Layer } from "effect"
import * as Schema from "@effect/schema/Schema"
import { WASMCalculationError, type WASMError } from "$lib/errors/wasm"
import { loadWASM } from "$lib/wasm/loader"
import type { Inputs, OptionPrice } from "$lib/wasm/types"

export const InputsSchema = Schema.Struct({
  spot: Schema.Number.pipe(
    Schema.positive({ message: () => "Spot price must be positive" }),
    Schema.finite({ message: () => "Spot price must be finite" }),
  ),
  strike: Schema.Number.pipe(
    Schema.positive({ message: () => "Strike price must be positive" }),
    Schema.finite({ message: () => "Strike price must be finite" }),
  ),
  timeToExpiry: Schema.Struct({
    days: Schema.Number.pipe(
      Schema.positive({ message: () => "Time to expiry must be positive" }),
      Schema.finite({ message: () => "Time to expiry must be finite" }),
    ),
  }),
  volatility: Schema.Number.pipe(
    Schema.positive({ message: () => "Volatility must be positive" }),
    Schema.finite({ message: () => "Volatility must be finite" }),
  ),
  riskFreeRate: Schema.Number.pipe(
    Schema.greaterThanOrEqualTo(0, {
      message: () => "Risk-free rate must be non-negative",
    }),
    Schema.finite({ message: () => "Risk-free rate must be finite" }),
  ),
  kind: Schema.Literal("Call", "Put"),
})

export const OptionPriceSchema = Schema.Struct({
  price: Schema.Number,
  greeks: Schema.Struct({
    delta: Schema.Number,
    gamma: Schema.Number,
    vega: Schema.Number,
    theta: Schema.Number,
    rho: Schema.Number,
  }),
})

export interface IWASMService {
  calculatePrice: (input: Inputs) => Effect.Effect<OptionPrice, WASMError>
}

export const WASMService = Context.GenericTag<IWASMService>("WASMService")

export const WASMServiceLive = Layer.effect(
  WASMService,
  Effect.gen(function* () {
    const wasm = yield* loadWASM()

    return WASMService.of({
      calculatePrice: input =>
        Effect.gen(function* () {
          const validated = yield* Schema.decodeUnknown(InputsSchema)(
            input,
          ).pipe(
            Effect.mapError(
              err =>
                new WASMCalculationError({
                  message: err.message,
                }),
            ),
          )

          const result = yield* Effect.try({
            try: () => wasm.calculateBlackScholes(validated),
            catch: cause =>
              new WASMCalculationError({
                message:
                  cause instanceof Error
                    ? cause.message
                    : "Failed to calculate option price",
                cause,
              }),
          })

          const validatedResponse = yield* Schema.decodeUnknown(
            OptionPriceSchema,
          )(result).pipe(
            Effect.mapError(
              err =>
                new WASMCalculationError({
                  message: `Invalid response: ${err.message}`,
                }),
            ),
          )

          return validatedResponse
        }),
    })
  }),
)
