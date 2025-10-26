import { describe, it, expect } from "vitest"
import { getErrorMessage, WASMLoadError, WASMCalculationError } from "./wasm"

describe("getErrorMessage", () => {
  it("returns generic error message for null", () => {
    const result = getErrorMessage(null)
    expect(result).toEqual({
      title: "Error",
      message: "An unknown error occurred",
    })
  })

  it("returns generic error message for undefined", () => {
    const result = getErrorMessage(undefined)
    expect(result).toEqual({
      title: "Error",
      message: "An unknown error occurred",
    })
  })

  it("returns WASM error for generic Error", () => {
    const error = new Error("Something went wrong")
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "WASM Error",
      message: "Something went wrong",
    })
  })

  it("returns loading error for WASMLoadError", () => {
    const error = new WASMLoadError({ cause: "Failed to fetch" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "WASM Loading Error",
      message:
        "Failed to load WASM module. Please refresh the page and try again.",
    })
  })

  it("returns calculation error for WASMCalculationError", () => {
    const error = new WASMCalculationError({ message: "Invalid input" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Calculation Error",
      message: "Invalid input",
    })
  })

  it("returns calculation error with fallback message when message is empty", () => {
    const error = new WASMCalculationError({ message: "" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Calculation Error",
      message: "Failed to calculate option price",
    })
  })
})
