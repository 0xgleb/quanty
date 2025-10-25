import { Context, Data, Effect, Layer } from "effect"
import * as Schema from "@effect/schema/Schema"
import type { Inputs, OptionPrice } from "$lib/api/generated/types.gen"
import { postBlackScholes } from "$lib/api/generated/sdk.gen"

// Error types for Black-Scholes service
export class NetworkError extends Data.TaggedError("NetworkError")<{
  cause: unknown
}> {}

export class ValidationError extends Data.TaggedError("ValidationError")<{
  message: string
}> {}

export class ApiError extends Data.TaggedError("ApiError")<{
  status: number
  message: string
}> {}

export type BlackScholesError = NetworkError | ValidationError | ApiError

// Schema for Black-Scholes inputs with validation
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

// Schema for option price response
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

// Black-Scholes service interface
export interface IBlackScholesService {
  calculatePrice: (
    input: Inputs,
  ) => Effect.Effect<OptionPrice, BlackScholesError>
}

// Service tag for dependency injection
export const BlackScholesService = Context.GenericTag<IBlackScholesService>(
  "BlackScholesService",
)

// Live implementation of the service
export const BlackScholesServiceLive = Layer.succeed(
  BlackScholesService,
  BlackScholesService.of({
    calculatePrice: input =>
      Effect.gen(function* () {
        // Validate input using schema
        const validated = yield* Schema.decodeUnknown(InputsSchema)(input).pipe(
          Effect.mapError(
            err =>
              new ValidationError({
                message: err.message,
              }),
          ),
        )

        // Make API call with timeout
        const result = yield* Effect.tryPromise({
          try: () =>
            postBlackScholes({
              body: validated,
            }),
          catch: err =>
            new NetworkError({
              cause: err,
            }),
        }).pipe(
          Effect.timeout("5 seconds"),
          Effect.flatMap(response => {
            // Handle HTTP errors
            if (!response.data) {
              if (response.error) {
                return Effect.fail(
                  new ApiError({
                    status: 400,
                    message: "Invalid request",
                  }),
                )
              }
              return Effect.fail(
                new ApiError({
                  status: 500,
                  message: "Unknown error",
                }),
              )
            }
            return Effect.succeed(response.data)
          }),
          Effect.catchTag("TimeoutException", () =>
            Effect.fail(
              new NetworkError({
                cause: new Error("Request timed out after 5 seconds"),
              }),
            ),
          ),
        )

        // Validate response using schema
        yield* Schema.decodeUnknown(OptionPriceSchema)(result).pipe(
          Effect.mapError(
            err =>
              new ValidationError({
                message: `Invalid response: ${err.message}`,
              }),
          ),
        )

        return result
      }),
  }),
)
