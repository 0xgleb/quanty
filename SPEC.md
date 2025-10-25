# Quanty Technical Specification

## Overview

Quanty is a web application for pricing cryptocurrency options and financial
derivatives. It provides an intuitive interface for quantitative analysts,
traders, and researchers to price options using various models, analyze Greeks,
and visualize risk.

The system is built with a Haskell backend for high-performance numerical
computation and a TypeScript/Svelte frontend for modern web UX.

## Architecture

### System Components

**Backend (Haskell + Servant)**:

- REST API server exposing pricing endpoints
- Pure functional pricing models (Black-Scholes, Binomial, Monte Carlo)
- Market data integration layer
- Caching layer for expensive computations

**Frontend (SvelteKit + TypeScript)**:

- Web application with interactive UI
- Option pricing calculators with real-time updates
- Financial charting with TradingView Lightweight Charts
- Greeks analysis and visualization

**Infrastructure (Nix)**:

- Reproducible development environment
- Unified dependency management for both Haskell and Node.js
- Build system for deployment

### Technology Stack

#### Backend

**Language**: Haskell with GHC2024 + Protolude

- **GHC2024** language baseline with modern type system features
- Strong static typing prevents runtime errors
- Pure functions enable easy testing and reasoning
- Lazy evaluation for efficient computation
- `NoImplicitPrelude` with Protolude for modern Haskell
- Modern record handling (`OverloadedRecordDot`, `DuplicateRecordFields`,
  `NoFieldSelectors`)

**Web Framework**: Servant with NamedRoutes

- **NamedRoutes** pattern for explicit, named API handlers
- Type-level API specification with better error messages
- Automatic API documentation generation
- Type-safe routing and request handling
- Built-in support for content negotiation (JSON, HTML)

**Key Libraries**: `servant-server`, `aeson`, `http-client`, `statistics`,
`vector`, `hspec`

#### Frontend

**Framework**: SvelteKit - Full-stack web framework with file-based routing

**UI Framework**: Svelte 5 - Reactive programming with runes (`$state`,
`$derived`, `$effect`)

**Component Library**: shadcn/ui (Svelte port) - Accessible, customizable
components

**Styling**: TailwindCSS - Utility-first CSS framework

**Type Safety**: TypeScript + Effect + @effect/schema - End-to-end type safety
with generated types from backend and runtime validation

**Error Handling**: Effect - Type-safe error handling with explicit error types

**Charting**: TradingView Lightweight Charts - Professional financial charting
library

## API Design

### REST API

All APIs use **Servant NamedRoutes** pattern for type-safe, explicit handler
naming. The API specification is defined in Haskell and automatically generates:

- OpenAPI 3.0 specification (`openapi.json`)
- TypeScript client types and SDK (via openapi-ts)

**Implemented Features:**

- Black-Scholes European option pricing with Greeks
- Health check endpoint

**Documentation:** OpenAPI spec available at `/openapi.json` when running the
server. Future: Swagger UI for interactive API documentation.

### Core Data Types

Domain types defined in Haskell:

- `OptionType`: Call | Put
- `OptionStyle`: European | American
- `BlackScholesInput`: Pricing parameters (spot, strike, time, rate, volatility)
- `OptionPrice`: Price with Greeks
- `Greeks`: Delta, Gamma, Vega, Theta, Rho (option sensitivities)

JSON serialization handled automatically via `aeson` with Generic deriving.

## Pricing Models

### Black-Scholes Model (Phase 1)

Classic closed-form solution for European options.

**Key Assumptions**:

- European exercise (only at expiration)
- No dividends, constant volatility, constant risk-free rate
- Lognormal price distribution, continuous trading
- No transaction costs

**Implementation Approach**:

- Calculate d1 and d2 parameters using standard formulas
- Use cumulative normal distribution (CDF) for pricing
- Calculate Greeks via analytical derivatives or finite differences

### Crypto-Specific Adjustments (Future)

Cryptocurrency markets have unique characteristics:

- **24/7 Trading**: 365 days/year vs 252 for traditional markets
- **High Volatility**: Much higher volatility than traditional assets
- **Thin Markets**: Lower liquidity can cause significant slippage
- **Funding Rates**: Perpetual futures funding affects option pricing
- **Regulatory Risk**: Sudden price movements from regulatory changes

Future iterations will incorporate:

- Jump-diffusion models for sudden price movements
- Stochastic volatility models (Heston)
- Crypto-specific calendar adjustments
- Funding rate incorporation for perpetual derivatives

## Frontend Architecture

### Component Structure

Organized by feature in `frontend/src/`:

- `lib/components/ui/` - shadcn/ui base components
- `lib/components/calculator/` - Option pricing form and results
- `lib/components/charts/` - Payoff diagrams, Greeks charts, volatility surface
- `lib/components/layout/` - Header, sidebar, navigation
- `lib/services/` - Effect services for API clients, config, market data, etc.
- `lib/schemas/` - Effect Schema definitions for data validation
- `lib/api/` - API client service implementation (Effect-based)
- `lib/stores/` - Svelte stores for state management
- `routes/` - SvelteKit file-based routes

### State Management

**Svelte 5 Runes**:

- `$state` - Reactive state variables
- `$derived` - Computed values that update automatically
- `$effect` - Side effects that run when dependencies change

State is managed locally in components with Svelte runes. Shared state uses
Svelte stores when needed across routes.

**Effect Integration**:

- Execute Effect programs with `Effect.runPromise` in components
- Integrate Effect async operations with Svelte runes
- Use Effect services for dependency injection
- Handle errors with `Effect.catchAll` and update Svelte state accordingly

### Type Safety Strategy

1. Define Servant API types in Haskell
2. Generate Effect Schemas from backend types (or manually define)
3. Use `Schema.decode` for runtime validation of all API responses
4. Frontend imports generated schemas and types
5. Compile-time verification of API contracts via TypeScript
6. Runtime validation via Effect Schema ensures type safety at boundaries
7. Use Schema refinements for business rules (positive numbers, valid ranges)
8. Define explicit error types for all failure modes

**Example Schema Definition**:

```typescript
import { Schema } from "@effect/schema";

export const OptionPriceSchema = Schema.Struct({
  price: Schema.Number.pipe(Schema.positive()),
  greeks: Schema.Struct({
    delta: Schema.Number,
    gamma: Schema.Number.pipe(Schema.positive()),
    vega: Schema.Number.pipe(Schema.positive()),
    theta: Schema.Number,
    rho: Schema.Number,
  }),
});

export type OptionPrice = Schema.Schema.Type<typeof OptionPriceSchema>;

// Decode API response with validation
const decodePrice = Schema.decodeUnknown(OptionPriceSchema);
const priceEffect = decodePrice(apiResponse); // Effect<OptionPrice, ParseError>
```

**Error Type Definition**:

```typescript
import { Data } from "effect";

export class ApiError extends Data.TaggedError("ApiError")<{
  readonly status: number;
  readonly message: string;
}> {}

export class ValidationError extends Data.TaggedError("ValidationError")<{
  readonly issues: ReadonlyArray<Schema.ParseIssue>;
}> {}
```

## Development Workflow

### Backend Development

Package by feature, not by layer. Each feature module contains all related code.

1. Create feature module (e.g., `src/BlackScholes/`)
2. Define types within the feature module
3. Implement business logic in the same module
4. Add API endpoints in the same module
5. Write tests co-located with implementation
6. Format with fourmolu and lint with hlint

### Frontend Development

1. Design component UI (Figma or directly in Svelte)
2. Define Effect Schemas for data structures
3. Implement Effect services for external dependencies
4. Implement with Svelte + shadcn components
5. Connect to API using Effect-based API client
6. Add error handling with Effect error types
7. Add tests (component tests, integration tests, service tests)
8. Format with prettier and lint with eslint

### Full-Stack Feature Example

Adding binomial tree pricing:

1. Backend: Create `src/Binomial/` module containing types, pricing logic, API
   endpoints, and tests
2. Schema generation: Define Effect Schemas for binomial pricing types
3. Frontend: Create Effect service for binomial API, define error types
4. Frontend: Create calculator component with Schema validation
5. Frontend: Integrate Effect service in component with error handling
6. Testing: End-to-end tests with real API calls and Effect test utilities

## Testing Strategy

### Backend (Haskell + HSpec)

- **Unit Tests**: Test individual pricing models
- **Property Tests**: QuickCheck for invariants (e.g., call-put parity)
- **API Tests**: Test Servant endpoints with servant-client

### Frontend (Vitest + Testing Library + Effect Test)

- **Component Tests**: Test UI components in isolation
- **Integration Tests**: Test complete user flows
- **Service Tests**: Test Effect services with `Layer.succeed` for mocking
- **Schema Tests**: Test validation with valid/invalid inputs
- **Error Handling Tests**: Test error scenarios with `Effect.fail`
- **API Client Tests**: Mock API responses using test layers

## Deployment

### Development

```bash
stack run              # Backend API server
cd frontend && pnpm dev  # Frontend dev server
```

### Production

**Build Process**:

1. Backend: `stack build --copy-bins` produces binary
2. Frontend: `pnpm build` produces static assets
3. Deploy backend binary and frontend static files
4. Configure reverse proxy (nginx) to route requests

**Deployment Options**:

- **Fly.io**: Docker deployment with global edge network
- **Railway**: Simple deployment with Nix support
- **Self-hosted**: systemd service + nginx reverse proxy

**Future**: Docker multi-stage build combining backend and frontend

## Security Considerations

**API Security**:

- Rate limiting to prevent abuse
- Input validation on all endpoints
- CORS configuration for frontend origin
- HTTPS only in production

**Frontend Security**:

- CSP headers to prevent XSS
- Sanitize user inputs
- Secure handling of API keys for market data (if needed)

**Data Validation**:

- Backend validates all numeric inputs (positive prices, valid ranges)
- Frontend validates before submission (better UX)
- Type system prevents invalid states

## Performance Considerations

**Backend**:

- Pure functions enable easy memoization
- Lazy evaluation for efficiency
- Parallel computation for Monte Carlo simulations
- Caching for expensive calculations

**Frontend**:

- TBD: Rendering strategy (SSR, CSR, or hybrid)
- Code splitting for smaller bundle sizes
- Debouncing for real-time parameter updates
- WebWorkers for intensive client-side calculations (future)

## Future Enhancements

### Advanced Pricing Models

- Binomial tree pricing for American options
- Monte Carlo simulation for path-dependent options
- Implied volatility calculation from market prices
- Additional models: Heston, jump-diffusion, local volatility

### Market Data Integration

- Real-time crypto prices from exchanges (Binance, Coinbase)
- Historical price data for backtesting
- Implied volatility surface
- Options chain data (when available)

### Portfolio Analysis

- Multi-option position builder
- Portfolio Greeks aggregation
- Strategy templates (straddles, spreads, etc.)
- Risk metrics (VaR, max drawdown)

### Real-Time Features

- WebSocket streaming for live market data
- Live price and Greeks updates
- Real-time chart updates
- Mobile-responsive design

## References

- Black, F., & Scholes, M. (1973). "The Pricing of Options and Corporate
  Liabilities"
- Hull, J. C. "Options, Futures, and Other Derivatives"
- Servant Documentation: https://docs.servant.dev
- SvelteKit Documentation: https://kit.svelte.dev
