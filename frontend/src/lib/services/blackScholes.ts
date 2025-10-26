import { Context, Data, Effect, Layer } from "effect"
import * as Schema from "@effect/schema/Schema"
import type { Inputs, OptionPrice } from "$lib/api/generated/types.gen"
import { postBlackScholes } from "$lib/api/generated/sdk.gen"
import { client } from "$lib/api/client"

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

export const getErrorMessage = (
  error: BlackScholesError | Error | null | undefined,
): {
  title: string
  message: string
} => {
  if (!error) return { title: "Error", message: "An unknown error occurred" }

  if (!("_tag" in error))
    return {
      title: "Calculation Error",
      message: error.message || "Failed to calculate option price",
    }

  switch (error._tag) {
    case "NetworkError":
      return {
        title: "Connection Error",
        message:
          "Unable to connect to the server. Please check your internet connection and try again.",
      }
    case "ValidationError":
      return {
        title: "Validation Error",
        message: error.message || "Invalid input provided",
      }
    case "ApiError":
      return {
        title: "Server Error",
        message:
          error.status >= 500
            ? "The server encountered an error. Please try again later."
            : error.message || "Invalid request",
      }
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

export const BlackScholesServiceLive = Layer.succeed(
  BlackScholesService,
  BlackScholesService.of({
    calculatePrice: input =>
      Effect.gen(function* () {
        const validated = yield* Schema.decodeUnknown(InputsSchema)(input).pipe(
          Effect.mapError(
            err =>
              new ValidationError({
                message: err.message,
              }),
          ),
        )

        const result = yield* Effect.tryPromise({
          try: () =>
            postBlackScholes({
              client,
              body: validated,
            }),
          catch: err =>
            new NetworkError({
              cause: err,
            }),
        }).pipe(
          Effect.timeout("5 seconds"),
          Effect.flatMap(response => {
            if (!response.data) {
              const statusFromResponse = response.response?.status
              return Effect.fail(
                new ApiError({
                  status: statusFromResponse ?? 500,
                  message: response.error ? "Invalid request" : "Unknown error",
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
