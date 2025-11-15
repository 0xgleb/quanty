import { Context, Data, Effect, Layer } from "effect"
import * as Schema from "@effect/schema/Schema"
import type { Inputs, OptionPrice } from "$lib/api/generated/types.gen"
import { WASMService, WASMServiceLive } from "./wasm"
import type { WASMError } from "$lib/errors/wasm"

export class NetworkError extends Data.TaggedError("NetworkError")<{
  cause: unknown
}> {}

export class ValidationError extends Data.TaggedError("ValidationError")<{
  message: string
}> {}

export type BlackScholesError = NetworkError | ValidationError

const formatBlackScholesError = (error: BlackScholesError) => {
  switch (error._tag) {
    case "NetworkError":
      return {
        title: "WASM Loading Error",
        message:
          "Failed to load calculation module. Please refresh the page and try again.",
      }
    case "ValidationError":
      return {
        title: "Validation Error",
        message: error.message || "Invalid input provided",
      }
  }
}

export const getErrorMessage = (
  error: unknown,
): {
  title: string
  message: string
} => {
  if (!error) return { title: "Error", message: "An unknown error occurred" }

  if (error instanceof NetworkError || error instanceof ValidationError) {
    return formatBlackScholesError(error)
  }

  if (error instanceof Error) {
    return {
      title: "Calculation Error",
      message: error.message || "Failed to calculate option price",
    }
  }

  return {
    title: "Error",
    message: "An unknown error occurred",
  }
}

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

export interface IBlackScholesService {
  calculatePrice: (
    input: Inputs,
  ) => Effect.Effect<OptionPrice, BlackScholesError>
}

export const BlackScholesService = Context.GenericTag<IBlackScholesService>(
  "BlackScholesService",
)

const mapWASMError = (error: WASMError): BlackScholesError => {
  if (error._tag === "WASMLoadError") {
    return new NetworkError({ cause: error.cause })
  }
  return new ValidationError({ message: error.message })
}

export const BlackScholesServiceWASM = Layer.effect(
  BlackScholesService,
  Effect.gen(function* () {
    const wasm = yield* WASMService

    return BlackScholesService.of({
      calculatePrice: input =>
        wasm.calculatePrice(input).pipe(Effect.mapError(mapWASMError)),
    })
  }),
).pipe(Layer.provide(WASMServiceLive))
