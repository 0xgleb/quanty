import { describe, it, expect, vi, beforeEach } from "vitest"
import { render } from "@testing-library/svelte"
import WasmTestPage from "./+page.svelte"

describe("WASM Test Page", () => {
  beforeEach(() => {
    // Clear any previous global flag
    delete (globalThis.window as any).__QUANTY_WASM_LOADED__
  })

  it("should only load WASM once and not infinite loop", async () => {
    // Track how many times helloWasm is called
    let helloWasmCallCount = 0
    let stdoutCallCount = 0

    // Mock the WASM module import
    vi.doMock("$lib/wasm/ghc_wasm_jsffi.js", () => ({
      default: () => ({
        // FFI glue mock
        newJSVal: () => 1,
        getJSVal: () => ({}),
        freeJSVal: () => {},
        scheduleWork: () => {},
      }),
    }))

    // Mock WebAssembly
    const mockHelloWasm = vi.fn(() => {
      helloWasmCallCount++
      console.log(`helloWasm called (count: ${helloWasmCallCount})`)
    })

    const mockHsInit = vi.fn()

    global.WebAssembly.instantiateStreaming = vi.fn().mockResolvedValue({
      instance: {
        exports: {
          hs_init: mockHsInit,
          helloWasm: mockHelloWasm,
          memory: { buffer: new ArrayBuffer(1024) },
        },
      },
    })

    // Mock fetch
    global.fetch = vi.fn().mockResolvedValue({
      ok: true,
      arrayBuffer: () => Promise.resolve(new ArrayBuffer(1024)),
    } as Response)

    // Render the component
    const { container } = render(WasmTestPage)

    // Wait a bit for async operations
    await new Promise(resolve => setTimeout(resolve, 100))

    // Check that helloWasm was called exactly once
    expect(helloWasmCallCount).toBe(1)
    expect(mockHelloWasm).toHaveBeenCalledTimes(1)

    console.log("Test passed! helloWasm was called exactly once")
  })

  it("should not trigger infinite loop when updating logs state", async () => {
    let renderCount = 0

    // We'll track re-renders by watching the DOM
    const { container } = render(WasmTestPage)

    // Set up a MutationObserver to count DOM updates
    const observer = new MutationObserver(() => {
      renderCount++
    })

    observer.observe(container, {
      childList: true,
      subtree: true,
      characterData: true,
    })

    // Wait for initial render and WASM load
    await new Promise(resolve => setTimeout(resolve, 200))

    // Stop observing
    observer.disconnect()

    // Should have a reasonable number of renders (not hundreds/thousands)
    console.log(`Render count: ${renderCount}`)
    expect(renderCount).toBeLessThan(20) // Should be much less than this
  })
})
