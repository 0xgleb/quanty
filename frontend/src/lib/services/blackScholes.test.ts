import { describe, it, expect } from "vitest"
import { getErrorMessage, NetworkError, ValidationError } from "./blackScholes"

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

  it("returns calculation error for generic Error", () => {
    const error = new Error("Something went wrong")
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Calculation Error",
      message: "Something went wrong",
    })
  })

  it("returns WASM loading error for NetworkError", () => {
    const error = new NetworkError({ cause: "WASM fetch failed" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "WASM Loading Error",
      message:
        "Failed to load calculation module. Please refresh the page and try again.",
    })
  })

  it("returns validation error for ValidationError", () => {
    const error = new ValidationError({ message: "Invalid input" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Validation Error",
      message: "Invalid input",
    })
  })

  it("returns validation error with fallback message when message is empty", () => {
    const error = new ValidationError({ message: "" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Validation Error",
      message: "Invalid input provided",
    })
  })
})
