import { Effect, Data } from "effect"
import * as Client from "./client"

export class ApiError extends Data.TaggedError("ApiError")<{
  readonly message: string
  readonly cause?: unknown
}> {}

export const getHealth = Effect.gen(function* () {
  const response = yield* Effect.tryPromise({
    try: () => Client.getHealth(),
    catch: error =>
      new ApiError({
        message: "Failed to fetch health status",
        cause: error,
      }),
  })

  if (!response.data) {
    return yield* Effect.fail(
      new ApiError({ message: "No data in health response" }),
    )
  }

  return response.data
})

export const getPlaceholder = Effect.gen(function* () {
  const response = yield* Effect.tryPromise({
    try: () => Client.getPlaceholder(),
    catch: error =>
      new ApiError({
        message: "Failed to fetch placeholder data",
        cause: error,
      }),
  })

  if (!response.data) {
    return yield* Effect.fail(
      new ApiError({ message: "No data in placeholder response" }),
    )
  }

  return response.data
})

export type { HealthResponse, PlaceholderResponse } from "./client"
