import { vi } from "vitest"
import type { HealthResponse, PlaceholderResponse } from "$lib/api/client"

export const createMockHealthResponse = (
  overrides: Partial<HealthResponse> = {},
): HealthResponse => ({
  status: "ok",
  version: "0.1.0",
  ...overrides,
})

export const createMockPlaceholderResponse = (
  overrides: Partial<PlaceholderResponse> = {},
): PlaceholderResponse => ({
  message: "Hello from the Haskell backend!",
  timestamp: "2024-01-01T00:00:00Z",
  ...overrides,
})

export const createMockApiResponse = <T>(
  data: T | null,
  error: Error | null = null,
) => ({
  data,
  error,
  response: {
    ok: error === null,
    status: error ? 500 : 200,
  } as Response,
})

export const mockGetHealth = vi.fn()
export const mockGetPlaceholder = vi.fn()
