# Serverless Architecture with Haskell WASM - Implementation Plan

## Overview

Migrate from Servant API backend to a serverless architecture using GHC's
WebAssembly backend. This will enable offline-first functionality, eliminate
backend hosting costs, and reduce network latency to zero for calculations.

**Current Architecture:** TypeScript Frontend ↔ HTTP/JSON ↔ Haskell Backend API

**Target Architecture:** TypeScript Frontend → JavaScript calls → Haskell WASM
Module

**Hybrid Approach:** Keep Svelte/TypeScript for UI/UX, use Haskell WASM for
numerical computation.

## Design Decisions

### Why WASM over API?

1. **Serverless deployment** - Deploy to Vercel/Netlify for free, no backend
   hosting
2. **Zero network latency** - Calculations run locally, instant results
3. **Offline-first** - PWA-ready, works without internet
4. **Code reuse** - Same Haskell code for web, CLI, desktop (potential future)

### Why Haskell WASM specifically?

1. **Keep existing code** - Black-Scholes implementation already works
2. **Type safety** - Leverage Haskell's type system for numerical computation
3. **Best of both worlds** - Haskell for computation, TypeScript for UI

### Bundle Size Tradeoff

- Current: 664KB
- With WASM: ~2MB (664KB + ~1.3MB WASM)
- Acceptable for a sophisticated numerical computation app
- Mitigations: wasm-opt, lazy loading, dead code elimination

### TypeScript Bindings Strategy

**No automatic generation** (unlike Rust's wasm-bindgen), but workable:

- Use `aeson-typescript` for data types (JSON-based communication)
- Write manual function declarations (one-time setup)
- Build script to keep types in sync

### Incremental Approach

Each phase has a clear go/no-go decision point:

- **Phase 1**: Does WASM load and work in browser?
- **Phase 2**: Is TypeScript integration ergonomic?
- **Phase 3**: Do calculations match API exactly?
- **Phase 4**: Is bundle size and performance acceptable?

---

## Task 1. Nix Development Environment Setup

Set up GHC WASM toolchain using Nix flakes with `ghc-wasm-meta`.

**Reasoning:** Nix provides reproducible builds and we're already using it.
`ghc-wasm-meta` is the official distribution of GHC WASM toolchain.

- [x] Add `ghc-wasm-meta` input to `flake.nix`
- [x] Add WASM toolchain to `devShells.default.packages`
  - `ghc-wasm-meta.packages.${system}.all_9_12`
  - Provides: `wasm32-wasi-ghc`, `wasm32-wasi-cabal`, `wasm32-wasi-ghc-pkg`
- [x] Test WASM toolchain availability
  - GHC WASM: version 9.12.2.20250924 ✓
  - Cabal: version 3.14.2.0 ✓
  - Post-linker: verified at expected location ✓
- [ ] Document WASM toolchain in README.md
  - Add section on WASM development
  - List available WASM commands

---

## Task 2. Minimal WASM Module (Hello World)

Create a minimal Haskell module that exports a single function to JavaScript and
verify it works in the browser.

**Reasoning:** Validate the entire toolchain before investing in complex
integration. This is our go/no-go checkpoint for the experiment.

- [ ] Create `wasm/` directory for WASM-specific code
  - Separate from main API code (different build target)
  - Structure: `wasm/quanty-wasm.cabal`, `wasm/Main.hs`
- [ ] Create minimal Cabal project for WASM target
  ```cabal
  executable quanty-wasm
    main-is: Main.hs
    build-depends: base
    default-language: Haskell2010
    ghc-options: -no-hs-main -O2
                 -optl-mexec-model=reactor
                 -optl-Wl,--export=hs_init
                 -optl-Wl,--export=helloWasm
                 -optl-Wl,--strip-all
                 -optl-Wl,--gc-sections
  ```
- [ ] Implement `helloWasm` function with JavaScript FFI

  ```haskell
  foreign export javascript "helloWasm"
    helloWasm :: IO ()

  helloWasm :: IO ()
  helloWasm = putStrLn "Hello from Haskell WASM!"
  ```

- [ ] Create build script `wasm/build.sh`
  - Compile: `wasm32-wasi-cabal build`
  - Copy WASM:
    `cp $(wasm32-wasi-cabal list-bin exe:quanty-wasm) dist/quanty.wasm`
  - Generate FFI glue:
    `post-link.mjs -i dist/quanty.wasm -o dist/ghc_wasm_jsffi.js`
- [ ] Add `@bjorn3/browser_wasi_shim` to frontend
  - `cd frontend && pnpm add @bjorn3/browser_wasi_shim`
- [ ] Create browser integration test page
  - New route: `frontend/src/routes/wasm-test/+page.svelte`
  - Load WASM module with WASI shim
  - Call `helloWasm()` and display result
- [ ] Verify WASM loads and executes in browser
  - Check browser console for "Hello from Haskell WASM!"
  - Measure bundle size of WASM module
  - Test in Chrome, Firefox, Safari
- [ ] Document findings
  - Bundle size (before and after wasm-opt)
  - Load time measurements
  - Browser compatibility

**Go/No-Go Decision:**

- ✅ Go if: WASM loads, function calls work, bundle size < 5MB
- ❌ No-go if: Too complex, bundle too large, browser compatibility issues

---

## Task 3. Bidirectional Data Passing (JSON)

Implement bidirectional data passing between TypeScript and Haskell using JSON
serialization.

**Reasoning:** We need to pass complex data structures (BlackScholesInput,
OptionPrice). JSON is the simplest approach that leverages existing Aeson
instances.

- [ ] Add `aeson` and `text` to WASM Cabal dependencies
- [ ] Implement JSON-based function in Haskell

  ```haskell
  import Data.Aeson (encode, decode)
  import Data.Text.Encoding (encodeUtf8, decodeUtf8)

  foreign export javascript "echoJson"
    echoJson :: JSVal -> IO JSVal

  echoJson :: JSVal -> IO JSVal
  echoJson input = do
    -- Convert JSVal to Text, parse JSON, serialize back
    -- Return JSVal
  ```

- [ ] Add JavaScript FFI helpers for JSVal conversion

  ```haskell
  foreign import javascript "JSON.stringify($1)"
    jsvalToString :: JSVal -> IO String

  foreign import javascript "JSON.parse($1)"
    stringToJSVal :: String -> IO JSVal
  ```

- [ ] Test roundtrip: TypeScript → JSON → Haskell → JSON → TypeScript
  - Send object from browser
  - Verify Haskell receives it correctly
  - Verify browser receives response
- [ ] Measure performance overhead of JSON serialization
  - Benchmark with realistic data sizes
  - Compare to current HTTP+JSON overhead

---

## Task 4. Type Generation with aeson-typescript

Generate TypeScript type definitions from Haskell ADTs to maintain type safety
across the WASM boundary.

**Reasoning:** Type safety is critical. Manual TypeScript definitions will drift
from Haskell types. Automated generation ensures they stay in sync.

- [ ] Add `aeson-typescript` to WASM project dependencies
- [ ] Derive `TypeScript` instance for existing types

  ```haskell
  import Data.Aeson.TypeScript.TH

  data BlackScholesInput = BlackScholesInput { ... }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJSON, FromJSON, TypeScript)
  ```

- [ ] Create type generation script `wasm/gen-types.hs`
  ```haskell
  main :: IO ()
  main = do
    writeTypeScriptDeclarations
      "frontend/src/lib/wasm/types.ts"
      (Proxy @BlackScholesInput)
    -- Add other types
  ```
- [ ] Integrate type generation into build process
  - Add to `wasm/build.sh`
  - Run before TypeScript compilation
- [ ] Update frontend to use generated types
  - Import from `lib/wasm/types.ts`
  - Remove manual type definitions
- [ ] Write manual function declarations

  ```typescript
  // lib/wasm/bindings.ts
  import type { BlackScholesInput, OptionPrice } from "./types";

  export interface QuantyWASM {
    calculateBlackScholes(input: BlackScholesInput): Promise<OptionPrice>;
  }
  ```

- [ ] Test type safety
  - Intentionally break types in Haskell
  - Verify TypeScript compilation fails

**Go/No-Go Decision:**

- ✅ Go if: Type generation works, ergonomic to use
- ❌ No-go if: Too much manual work, types don't match

---

## Task 5. Port Black-Scholes to WASM

Implement Black-Scholes calculation in WASM module using existing Haskell code.

**Reasoning:** Reuse existing, tested Black-Scholes implementation. Just need to
add FFI exports.

- [ ] Copy Black-Scholes types to WASM module
  - `wasm/BlackScholes/Types.hs`
  - Add `TypeScript` deriving to all types
- [ ] Copy Black-Scholes calculation to WASM module
  - `wasm/BlackScholes/Calculate.hs`
  - Pure functions, no changes needed
- [ ] Add FFI export for `calculateBlackScholes`

  ```haskell
  foreign export javascript "calculateBlackScholes"
    calculateBlackScholesFFI :: JSVal -> IO JSVal

  calculateBlackScholesFFI :: JSVal -> IO JSVal
  calculateBlackScholesFFI inputJS = do
    inputStr <- jsvalToString inputJS
    case decode (encodeUtf8 $ toS inputStr) of
      Nothing -> error "Invalid input"  -- TODO: proper error handling
      Just input -> do
        let result = calculateBlackScholes input
        stringToJSVal (toS $ decodeUtf8 $ encode result)
  ```

- [ ] Update type generation to include all Black-Scholes types
  - `BlackScholesInput`, `OptionPrice`, `Greeks`, `OptionType`, etc.
- [ ] Rebuild WASM module with Black-Scholes
- [ ] Verify calculation correctness
  - Test with known inputs
  - Compare results to current API implementation

---

## Task 6. WASM Loader and Effect Integration

Create a WASM loader service and integrate with Effect architecture for
type-safe async operations.

**Reasoning:** Match existing Effect-based architecture. WASM loading is async
and can fail, perfect fit for Effect.

- [ ] Create WASM loader utility

  ```typescript
  // lib/wasm/loader.ts
  import { WASI } from "@bjorn3/browser_wasi_shim";
  import type { QuantyWASM } from "./bindings";

  export async function loadQuantyWASM(): Promise<QuantyWASM> {
    const wasi = new WASI([], [], []);
    const instance_exports = {};

    const { instance } = await WebAssembly.instantiateStreaming(
      fetch("/quanty.wasm"),
      {
        wasi_snapshot_preview1: wasi.wasiImport,
        ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
      },
    );

    Object.assign(instance_exports, instance.exports);
    wasi.initialize(instance);
    await instance.exports.hs_init();

    return {
      calculateBlackScholes: async (input) => {
        // Call WASM function, parse result
      },
    };
  }
  ```

- [ ] Create WASMService with Effect

  ```typescript
  // lib/services/wasm.ts
  import { Context, Effect, Layer } from "effect";

  export class WASMService extends Context.Tag("WASMService")<
    WASMService,
    {
      readonly calculateBlackScholes: (
        input: BlackScholesInput,
      ) => Effect.Effect<OptionPrice, WASMError>;
    }
  >() {}

  export const WASMServiceLive = Layer.effect(
    WASMService,
    Effect.gen(function* (_) {
      const wasm = yield* _(Effect.promise(() => loadQuantyWASM()));

      return {
        calculateBlackScholes: (input) =>
          Effect.tryPromise({
            try: () => wasm.calculateBlackScholes(input),
            catch: (error) => new WASMError({ error }),
          }),
      };
    }),
  );
  ```

- [ ] Define WASM error types

  ```typescript
  // lib/errors/wasm.ts
  import { Data } from "effect";

  export class WASMLoadError extends Data.TaggedError("WASMLoadError")<{
    readonly error: unknown;
  }> {}

  export class WASMCalculationError extends Data.TaggedError(
    "WASMCalculationError",
  )<{
    readonly error: unknown;
  }> {}

  export type WASMError = WASMLoadError | WASMCalculationError;
  ```

- [ ] Add singleton pattern for WASM instance
  - WASM module should load once, reuse across calls
  - Cache in Layer for dependency injection
- [ ] Add loading state handling
  - Show loading indicator while WASM initializes
  - Handle initialization errors gracefully

---

## Task 7. Update BlackScholesService to use WASM

Replace HTTP API calls with WASM calls in the existing BlackScholesService.

**Reasoning:** Maintain existing service interface, just change the
implementation. This allows easy rollback if needed.

- [ ] Update BlackScholesService to use WASMService

  ```typescript
  // lib/services/black-scholes.ts
  export const BlackScholesServiceLive = Layer.effect(
    BlackScholesService,
    Effect.gen(function* (_) {
      const wasm = yield* _(WASMService);

      return {
        calculate: (input) =>
          wasm
            .calculateBlackScholes(input)
            .pipe(Effect.mapError() /* map WASM errors to API errors */),
      };
    }),
  ).pipe(Layer.provide(WASMServiceLive));
  ```

- [ ] Add error mapping
  - Map `WASMError` to existing `ApiError` types
  - Preserve error handling behavior
- [ ] Update App component to provide WASMService layer
  - Replace `BlackScholesServiceLive` (HTTP) with WASM version
- [ ] Test calculator UI with WASM backend
  - All existing functionality should work
  - No UI changes needed
- [ ] Add feature flag for WASM vs API

  ```typescript
  const BlackScholesServiceLive = import.meta.env.VITE_USE_WASM
    ? BlackScholesServiceWASM
    : BlackScholesServiceHTTP;
  ```

  - Allow toggling between implementations
  - Useful for testing and gradual rollout

---

## Task 8. Verification and Testing

Verify WASM implementation produces identical results to current API and add
comprehensive tests.

**Reasoning:** Numerical correctness is critical. Must prove WASM calculations
match API exactly.

- [ ] Create comparison test suite
  - Run same inputs through both WASM and API
  - Compare results (should be identical)
  - Test all presets (ATM Call, OTM Call, ITM Put)
- [ ] Add property tests for WASM bindings
  - Roundtrip: Haskell → JSON → TypeScript → JSON → Haskell
  - Test edge cases (zero volatility, negative values, etc.)
- [ ] Test error handling
  - Invalid input JSON
  - WASM load failures
  - Calculation errors
- [ ] Verify all 50 existing frontend tests still pass
  - Should pass without changes (same service interface)
- [ ] Add WASM-specific tests
  - WASM module loads successfully
  - Multiple calculations work (no state corruption)
  - Concurrent calculations work (if applicable)
- [ ] Performance benchmarking
  - Measure calculation time: WASM vs API
  - Measure memory usage
  - Test with rapid-fire calculations

---

## Task 9. Bundle Optimization

Optimize WASM bundle size and loading performance.

**Reasoning:** 2MB WASM bundle is acceptable but should be optimized. Goal: <
3MB total bundle, < 2s initial load on 3G.

- [ ] Add `wasm-opt` to build process

  ```bash
  wasm-opt -O3 -o dist/quanty.opt.wasm dist/quanty.wasm
  ```

  - Should reduce size by ~15-20%

- [ ] Measure bundle sizes
  - Before optimization
  - After wasm-opt
  - After gzip (what CDN serves)
- [ ] Implement lazy loading for WASM module
  - Don't load WASM on initial page load
  - Load when user navigates to calculator
  - Show loading state while initializing
- [ ] Add cache headers for WASM file
  - Configure Vite to set proper cache headers
  - WASM module can be cached aggressively (immutable)
- [ ] Test on slow network
  - Simulate 3G connection
  - Measure time to interactive
  - Should be < 2s on 3G
- [ ] Document bundle sizes
  - Update README with before/after
  - Add to architecture documentation

**Success Criteria:**

- Total bundle (JS + WASM) < 3MB
- Initial page load < 2s on 3G
- WASM module cached properly

---

## Task 10. Vite Integration

Integrate WASM build process into Vite development workflow.

**Reasoning:** Seamless dev experience. WASM should rebuild automatically like
TypeScript does.

- [ ] Create Vite plugin for WASM build
  ```typescript
  // vite-plugin-wasm-build.ts
  export function wasmBuild(): Plugin {
    return {
      name: "wasm-build",
      async buildStart() {
        // Run wasm/build.sh
      },
      configureServer(server) {
        // Watch wasm/ directory
        // Rebuild on changes
      },
    };
  }
  ```
- [ ] Add WASM build to Vite config
  ```typescript
  // vite.config.ts
  export default defineConfig({
    plugins: [sveltekit(), wasmBuild()],
  });
  ```
- [ ] Configure WASM asset handling
  - Ensure `.wasm` files are copied to output
  - Configure proper MIME type (`application/wasm`)
- [ ] Test hot reload
  - Change Haskell code
  - Verify WASM rebuilds automatically
  - Verify browser reloads (may need manual refresh)
- [ ] Add development scripts
  - `pnpm dev` - should build WASM and start dev server
  - `pnpm build` - should build WASM and bundle for production
  - `pnpm wasm:build` - manual WASM rebuild

---

## Task 11. Error Handling and Edge Cases

Implement comprehensive error handling for production use.

**Reasoning:** WASM errors can be cryptic. Need user-friendly error messages and
graceful degradation.

- [ ] Improve Haskell error handling

  ```haskell
  data CalculationError
    = InvalidInput Text
    | NumericalError Text
    | InternalError Text
    deriving (Generic, Show, ToJSON, TypeScript)

  calculateBlackScholesFFI :: JSVal -> IO JSVal
  calculateBlackScholesFFI inputJS = do
    result <- try $ do
      -- parse input, calculate, serialize result
    case result of
      Left err -> return $ encodeError err
      Right val -> return val
  ```

- [ ] Map Haskell errors to TypeScript errors

  ```typescript
  export class InvalidInputError extends Data.TaggedError("InvalidInputError")<{
    readonly message: string;
  }> {}

  export class NumericalError extends Data.TaggedError("NumericalError")<{
    readonly message: string;
  }> {}
  ```

- [ ] Add user-friendly error messages
  - "Invalid input: spot price must be positive"
  - "Calculation failed: volatility too high"
  - "WASM module failed to load, please refresh"
- [ ] Handle WASM initialization failures
  - Show error page with reload button
  - Add fallback to API if available
  - Log errors to console for debugging
- [ ] Test error scenarios
  - WASM file not found (404)
  - WASM file corrupted
  - Invalid JSON from TypeScript
  - Numerical errors (divide by zero, sqrt of negative)
- [ ] Add Sentry or error tracking
  - Log WASM errors to error tracking service
  - Include context (input values, browser info)

---

## Task 12. Documentation and Cleanup

Update documentation to reflect new architecture and remove API backend code.

**Reasoning:** Documentation must match implementation. Remove dead code to
avoid confusion.

- [ ] Update SPEC.md
  - Replace API architecture section with WASM architecture
  - Update diagrams to show TypeScript → WASM flow
  - Document WASM build process
  - Update type definitions section
- [ ] Update README.md
  - Update "Getting Started" with WASM setup
  - Update build commands
  - Add WASM development section
  - Update deployment instructions (Vercel/Netlify)
- [ ] Update ROADMAP.md
  - Mark "Experimental: Serverless Architecture" as complete
  - Link to this PR
  - Update future phases to assume WASM backend
- [ ] Remove API backend code
  - Delete `src/Api.hs` (Servant API)
  - Delete `app/Main.hs` (API server)
  - Keep `src/BlackScholes/` types and logic (moved to `wasm/`)
  - Update `package.yaml` to remove executable
- [ ] Update frontend API client
  - Remove HTTP client code (`lib/api/client.ts`)
  - Keep only WASM service
- [ ] Add WASM development guide
  - How to build WASM module
  - How to add new functions
  - How to debug WASM issues
  - Type generation workflow
- [ ] Update .gitignore
  - Ignore `wasm/dist/`
  - Ignore generated types (if not committed)
- [ ] Update CI/CD
  - Remove backend deployment steps
  - Add WASM build step
  - Deploy to Vercel as static site

---

## Task 13. Deployment and Production Testing

Deploy to production (Vercel) and validate the entire system works end-to-end.

**Reasoning:** Final validation in production environment. Catch any
deployment-specific issues.

- [ ] Configure Vercel deployment
  - Create `vercel.json` if needed
  - Ensure WASM build runs in Vercel build step
  - Configure static asset serving
- [ ] Test production build locally
  - `pnpm build`
  - `pnpm preview`
  - Verify WASM loads correctly
  - Verify calculations work
- [ ] Deploy to Vercel preview
  - Create preview deployment
  - Test in preview environment
- [ ] Validate production deployment
  - Test calculator with all presets
  - Verify offline functionality (PWA)
  - Test on mobile devices
  - Test on different browsers (Chrome, Firefox, Safari)
- [ ] Performance validation
  - Measure bundle size in production
  - Measure load time on 3G
  - Verify < 3MB bundle, < 2s load time
- [ ] Add monitoring
  - Track WASM load failures
  - Track calculation errors
  - Track performance metrics
- [ ] Update live demo link
  - Update README with new Vercel URL
  - Test that demo works for first-time users

---

## Success Criteria

Final checklist before marking experiment as complete:

- ✅ WASM module loads in browser successfully
- ✅ TypeScript can call Haskell functions with full type safety
- ✅ Calculations produce identical results to previous API
- ✅ All 50+ existing tests pass
- ✅ Total bundle size < 3MB (including WASM)
- ✅ Initial page load < 2s on 3G
- ✅ Offline functionality works (PWA-ready)
- ✅ No backend deployment needed (static site only)
- ✅ Deployed to Vercel and accessible
- ✅ Documentation updated and accurate
- ✅ All checks pass (format, lint, tests, build)

---

## Rollback Plan

If the experiment fails at any phase:

**Phase 1-2 Failure (PoC or Types):**

- Delete `wasm/` directory
- Remove `ghc-wasm-meta` from `flake.nix`
- Close issue #12 with findings
- No impact on existing codebase

**Phase 3-4 Failure (After Integration):**

- Keep feature flag for WASM vs API
- Default to API backend
- Keep WASM as experimental opt-in feature
- Document findings in issue #12

**Severe Issues (Production):**

- Revert to API backend in emergency
- Feature flag can toggle between implementations
- Redeploy backend API to Fly.io/Railway
- WASM code can remain as experimental path
