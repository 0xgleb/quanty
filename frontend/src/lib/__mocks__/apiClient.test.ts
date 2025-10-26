import { describe, it, expect } from "vitest"
import {
  createMockHealthResponse,
  createMockPlaceholderResponse,
  createMockApiResponse,
} from "./apiClient"

describe("API Client Mocks", () => {
  describe("createMockHealthResponse", () => {
    it("creates a default health response", () => {
      const response = createMockHealthResponse()

      expect(response).toEqual({
        status: "ok",
        version: "0.1.0",
      })
    })

    it("allows overriding default values", () => {
      const response = createMockHealthResponse({ version: "1.0.0" })

      expect(response.version).toBe("1.0.0")
      expect(response.status).toBe("ok")
    })
  })

  describe("createMockPlaceholderResponse", () => {
    it("creates a default placeholder response", () => {
      const response = createMockPlaceholderResponse()

      expect(response).toHaveProperty("message")
      expect(response).toHaveProperty("timestamp")
      expect(response.message).toBe("Hello from the Haskell backend!")
    })

    it("allows overriding default values", () => {
      const response = createMockPlaceholderResponse({
        message: "Custom message",
        timestamp: "2024-01-01T00:00:00Z",
      })

      expect(response.message).toBe("Custom message")
      expect(response.timestamp).toBe("2024-01-01T00:00:00Z")
    })
  })

  describe("createMockApiResponse", () => {
    it("creates a successful API response", () => {
      const data = { test: "data" }
      const response = createMockApiResponse(data)

      expect(response.data).toEqual(data)
      expect(response.error).toBeNull()
      expect(response.response.ok).toBe(true)
      expect(response.response.status).toBe(200)
    })

    it("creates an error API response", () => {
      const error = new Error("Test error")
      const response = createMockApiResponse(null, error)

      expect(response.data).toBeNull()
      expect(response.error).toBe(error)
      expect(response.response.ok).toBe(false)
      expect(response.response.status).toBe(500)
    })
  })
})
