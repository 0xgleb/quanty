# Black-Scholes Calculator Implementation Plan

This plan covers implementing a complete Black-Scholes options pricing
calculator with backend computation and frontend UI.

## Scope

Implement the most basic version of Black-Scholes calculator where:

- User provides all inputs (spot price, strike, time to expiry, volatility,
  risk-free rate, option type)
- Backend performs pricing calculation using Black-Scholes model
- Frontend provides clean UI for entering parameters and displaying results
- Full type safety from backend to frontend via OpenAPI

## Task 1. Backend - Black-Scholes Module

Create `src/BlackScholes.hs` module with types, pricing logic, and tests.

- [x] Create `src/BlackScholes.hs` with module structure
- [x] Define core types:
  - `OptionType` (Call | Put)
  - `TimeToExpiryYears` newtype for explicit units
  - `BlackScholesInput` (spot, strike, timeToExpiry, volatility, riskFreeRate,
    kind)
  - `Greeks` (delta, gamma, vega, theta, rho)
  - `OptionPrice` (price, greeks)
- [x] Implement Black-Scholes pricing formula:
  - `calculatePrice :: BlackScholesInput -> OptionPrice`
  - d1 and d2 calculations
  - Call and Put pricing
  - Cumulative normal distribution (CDF)
- [x] Implement Greeks calculations:
  - Delta ($\partial V/\partial S$)
  - Gamma ($\partial^2 V/\partial S^2$)
  - Vega ($\partial V/\partial \sigma$)
  - Theta ($\partial V/\partial t$)
  - Rho ($\partial V/\partial r$)
- [x] Add JSON serialization (ToJSON/FromJSON instances)
- [x] Add OpenAPI schema instances (ToSchema)
- [x] Create `test/BlackScholesSpec.hs` with comprehensive tests:
  - Test known values (ATM options, ITM, OTM)
  - Test Call-Put parity property
  - Test Greeks ranges (delta, gamma, vega, theta, rho)
  - QuickCheck property tests with realistic parameter ranges
- [x] Run `stack build --fast` and `stack test --fast` to verify

**Reasoning**: Package-by-feature approach - all Black-Scholes code lives in one
module. We implement the mathematical model first with comprehensive testing
before exposing it via API. Using standard Black-Scholes formula for European
options as it's the foundation for options pricing.

## Task 2. Backend - API Endpoint

Add Black-Scholes pricing endpoint to the Servant API.

- [x] Update `src/Api.hs` to add `blackScholes` endpoint:
  - Route: `POST /black-scholes`
  - Request: `BlackScholes.Inputs` (JSON)
  - Response: `BlackScholes.OptionPrice` (JSON)
- [x] Import `BlackScholes` module types
- [x] Implement handler that calls `BlackScholes.calculatePriceWithGreeks`
- [x] Add input validation in handler:
  - Spot price > 0
  - Strike price > 0
  - Time to expiry > 0
  - Volatility > 0
  - All values are finite (no NaN/Infinity)
- [x] Add error handling for invalid inputs (return 400 with error message)
- [x] Update OpenAPI spec generation to include new endpoint
- [x] Verify `stack build --fast` succeeds
- [x] Verify `stack test --fast` passes

**Reasoning**: Using NamedRoutes pattern consistent with existing API. POST
request is appropriate since we're performing a computation (not idempotent).
Input validation at API layer protects business logic from invalid data.

## Task 3. Frontend - Generate TypeScript Client

Generate TypeScript types and client from OpenAPI spec.

- [x] Run `stack exec generate-openapi` to generate `openapi.json`
- [x] Run `pnpm generate-client` to generate TypeScript client
- [x] Verify generated types include:
  - `Inputs` interface
  - `OptionPrice` interface
  - `Greeks` interface
  - `OptionKind` type ('Call' | 'Put')
  - `postBlackScholes` function
- [x] Ensure `.gitignore` excludes generated client code (already present)
- [x] Add note in README about regenerating client after API changes

**Reasoning**: Auto-generation ensures type safety from backend to frontend. Any
changes to backend types automatically flow to frontend after regeneration.
Generated code should never be committed - it's regenerated on each build.

## Task 4. Frontend - Effect Service

Create Effect-based service for Black-Scholes API calls.

- [x] Create `frontend/src/lib/services/blackScholes.ts`
- [x] Define Effect Schemas for request/response validation:
  - `InputsSchema` with refinements (positive numbers, finite values)
  - `OptionPriceSchema` for response validation
- [x] Create `BlackScholesService` using `Context.Tag`
- [x] Implement `calculatePrice` function returning
      `Effect<OptionPrice, BlackScholesError>`
- [x] Add timeout (5 seconds) with `Effect.timeout`
- [x] Add error handling with proper error types:
  - `NetworkError` for connection issues
  - `ValidationError` for invalid inputs
  - `ApiError` for server errors
- [x] Create `BlackScholesServiceLive` Layer
- [x] Export service interface
- [x] Run `pnpm lint` and `pnpm format`

**Reasoning**: Effect provides type-safe async operations with explicit error
handling. Schema validation at runtime catches issues with external data.
Timeout prevents hanging requests. Layers enable dependency injection for
testing.

## Task 5. Frontend - Calculator UI Component

Build calculator form with parameter inputs.

- [ ] Create `frontend/src/routes/calculator/+page.svelte`
- [ ] Add form with shadcn-svelte components:
  - Option Type selector (Call/Put radio buttons or select)
  - Spot Price input (number)
  - Strike Price input (number)
  - Time to Expiry input (number, in years)
  - Volatility input (number, as decimal e.g., 0.25 for 25%)
  - Risk-Free Rate input (number, as decimal e.g., 0.05 for 5%)
- [ ] Use Svelte 5 runes for form state (`$state`)
- [ ] Create mutation using `createMutation` from Effect query system
- [ ] Wire up form submission to call `BlackScholesService.calculatePrice`
- [ ] Add loading state while calculation runs
- [ ] Display validation errors from Schema
- [ ] Add helpful labels and descriptions for each input
- [ ] Add input placeholders with example values
- [ ] Run `pnpm lint` and `pnpm format`

**Reasoning**: Form-first approach for calculator UI. shadcn-svelte provides
accessible, styled components. Svelte 5 runes give reactive state management.
Effect mutation handles async submission with proper loading/error states.

## Task 6. Frontend - Results Display

Display pricing results and Greeks.

- [ ] Add results section in calculator page (below form)
- [ ] Display option price prominently (large text)
- [ ] Create Greeks table showing all five Greeks:
  - Delta with description
  - Gamma with description
  - Vega with description
  - Theta with description
  - Rho with description
- [ ] Add tooltips explaining what each Greek measures
- [ ] Show results only after successful calculation
- [ ] Format numbers appropriately:
  - Price: 2-4 decimal places
  - Greeks: scientific notation if very small
- [ ] Add units/context for each value
- [ ] Run `pnpm lint` and `pnpm format`

**Reasoning**: Clear visual hierarchy with price as primary result and Greeks as
secondary details. Educational tooltips help users understand the outputs.
Conditional rendering prevents showing stale data.

## Task 7. Frontend - Error Handling & Polish

Add comprehensive error handling and UX improvements.

- [ ] Handle network errors with user-friendly messages
- [ ] Handle validation errors by showing field-specific errors
- [ ] Handle API errors (500, etc.) with generic error message
- [ ] Add error boundary with `Effect.catchAll`
- [ ] Add reset button to clear form and results
- [ ] Add example preset buttons (e.g., "ATM Call", "ITM Put")
- [ ] Ensure responsive design works on mobile
- [ ] Add page title and meta description
- [ ] Run `pnpm format` and `pnpm lint`
- [ ] Run `pnpm check` (TypeScript type check)

**Reasoning**: Graceful error handling improves UX. Preset buttons help users
get started quickly. Responsive design ensures usability on all devices.
Linting/formatting maintains code quality.

## Task 8. Testing

Add tests for frontend calculator.

- [ ] Create `frontend/src/routes/calculator/__tests__/calculator.test.ts`
- [ ] Test form rendering
- [ ] Test input validation (negative values rejected)
- [ ] Test successful calculation flow with mocked service
- [ ] Test error handling with mocked failures
- [ ] Use `Layer.succeed` to mock `BlackScholesService`
- [ ] Run `pnpm test:run`
- [ ] Ensure all tests pass

**Reasoning**: Tests ensure calculator works correctly and handles errors
gracefully. Mocking Effect services allows testing without backend dependency.

## Task 9. Documentation Updates

Update project documentation to reflect new feature.

- [ ] Update README.md:
  - Add "Features" section mentioning Black-Scholes calculator
  - Add screenshot or description of calculator UI
  - Document API endpoint in API section
- [ ] Update SPEC.md:
  - Add Black-Scholes types documentation
  - Add API endpoint specification
  - Add formula reference
- [ ] Update ROADMAP.md:
  - Mark Phase 1 tasks as complete
  - Mark Phase 2 calculator tasks as complete
  - Add PR link to completed tasks
- [ ] Delete PLAN.md (this file) using `git rm PLAN.md`

**Reasoning**: Documentation must stay in sync with code. ROADMAP.md tracks
progress. PLAN.md is temporary and must be deleted before PR. README and SPEC
provide long-term documentation.

## Testing Strategy

**Backend Tests** (HSpec + QuickCheck):

- Known value tests (e.g., ATM option with specific inputs)
- Call-Put parity: $C - P = S - Ke^{-rT}$ for European options
- Boundary conditions: zero volatility, zero time to expiry
- Greeks relationships: verify formulas match known derivatives
- Property tests: price always positive, delta in [0,1] for calls

**Frontend Tests** (Vitest + Testing Library):

- Component rendering with various states
- Form validation (Schema refinements)
- Successful calculation flow
- Error scenarios (network, validation, API errors)
- Mocked Effect services using Layers

## Technical Decisions

**Why Black-Scholes first?**

- Industry standard for European options pricing
- Closed-form solution (fast, deterministic)
- Foundation for understanding other models
- Simple enough for MVP

**Why package-by-feature?**

- All Black-Scholes code in one place (`src/BlackScholes.hs`)
- Easy to find related code (types, logic, tests together)
- Avoids generic `Types/` or `Models/` modules
- Better encapsulation and modularity

**Why Effect on frontend?**

- Type-safe error handling (no try/catch)
- Explicit error types in function signatures
- Composable async operations
- Runtime validation with Schema
- Testability via dependency injection (Layers)

**Why POST for pricing endpoint?**

- Pricing is a computation (not idempotent)
- Request body allows complex input structure
- Consistent with REST semantics for operations
- Easier to extend inputs in future

## Out of Scope

The following are explicitly **not** included in this implementation:

- American option pricing (requires binomial/Monte Carlo)
- Implied volatility calculation (Phase 5)
- Market data integration (Phase 4)
- Charts and visualizations (Phase 3)
- Multiple pricing models (Phase 5)
- Real-time updates (Phase 7)
- Strategy analysis (Phase 6)

These features are planned for later phases per ROADMAP.md.
