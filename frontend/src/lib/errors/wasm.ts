import { Data } from "effect"

export class WASMLoadError extends Data.TaggedError("WASMLoadError")<{
  readonly cause: unknown
}> {}

export class WASMCalculationError extends Data.TaggedError(
  "WASMCalculationError",
)<{
  readonly message: string
  readonly cause?: unknown
}> {}

export type WASMError = WASMLoadError | WASMCalculationError

export const getErrorMessage = (
  error: WASMError | Error | null | undefined,
): {
  title: string
  message: string
} => {
  if (!error) return { title: "Error", message: "An unknown error occurred" }

  if (!("_tag" in error))
    return {
      title: "WASM Error",
      message: error.message || "Failed to execute WASM calculation",
    }

  switch (error._tag) {
    case "WASMLoadError":
      return {
        title: "WASM Loading Error",
        message:
          "Failed to load WASM module. Please refresh the page and try again.",
      }
    case "WASMCalculationError":
      return {
        title: "Calculation Error",
        message: error.message || "Failed to calculate option price",
      }
  }
}
