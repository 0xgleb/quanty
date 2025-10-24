import "@testing-library/jest-dom/vitest"
import { vi } from "vitest"

vi.mock("$app/stores", async () => {
  const { readable, writable } = await import("svelte/store")
  const getStores = () => ({
    navigating: readable(null),
    page: readable({
      url: new URL("http://localhost"),
      params: {},
      searchParams: new URLSearchParams(),
    }),
    session: writable(null),
    updated: readable(false),
  })

  const page = readable({
    url: new URL("http://localhost"),
    params: {},
    searchParams: new URLSearchParams(),
  })
  const navigating = readable(null)
  const updated = readable(false)

  return { getStores, navigating, page, updated }
})

vi.mock("$app/navigation", () => ({
  goto: vi.fn(),
  invalidate: vi.fn(),
  invalidateAll: vi.fn(),
  preloadData: vi.fn(),
  preloadCode: vi.fn(),
  beforeNavigate: vi.fn(),
  afterNavigate: vi.fn(),
}))

vi.mock("$app/environment", () => ({
  browser: false,
  dev: true,
  building: false,
  version: "test",
}))
