import { describe, it, expect } from "vitest"
import {
  getErrorMessage,
  NetworkError,
  ValidationError,
  ApiError,
} from "./blackScholes"

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

  it("returns connection error for NetworkError", () => {
    const error = new NetworkError({ cause: "Network failure" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Connection Error",
      message:
        "Unable to connect to the server. Please check your internet connection and try again.",
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

  it("returns server error for ApiError with 500 status", () => {
    const error = new ApiError({ status: 500, message: "Internal error" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Server Error",
      message: "The server encountered an error. Please try again later.",
    })
  })

  it("returns server error for ApiError with 503 status", () => {
    const error = new ApiError({ status: 503, message: "Service unavailable" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Server Error",
      message: "The server encountered an error. Please try again later.",
    })
  })

  it("returns specific message for ApiError with 400 status", () => {
    const error = new ApiError({ status: 400, message: "Bad request" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Server Error",
      message: "Bad request",
    })
  })

  it("returns fallback message for ApiError with 400 status and empty message", () => {
    const error = new ApiError({ status: 400, message: "" })
    const result = getErrorMessage(error)
    expect(result).toEqual({
      title: "Server Error",
      message: "Invalid request",
    })
  })
})
