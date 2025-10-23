# AGENTS.md

This file provides guidance to AI agents working with code in this repository.

## Project Overview

Quanty is a web application for pricing cryptocurrency options and financial
derivatives. It combines a Haskell backend for high-performance numerical
computation with a TypeScript/Svelte frontend for modern web UX.

**Key Technologies**:

- **Backend**: Haskell + Servant for REST API
- **Frontend**: SvelteKit + TypeScript + shadcn/ui
- **Tooling**: Nix flakes for reproducible development environment
- **Language**: Haskell uses Protolude (no implicit Prelude)

**Important**: See [README.md](README.md) for getting started guide,
[SPEC.md](SPEC.md) for detailed architecture, and [ROADMAP.md](ROADMAP.md) for
development plan.

## Development Environment

This project uses Nix flakes with direnv for reproducible development:

```bash
direnv allow  # or: nix develop --impure
```

The development shell provides:

- GHC + Stack for Haskell development
- Node.js + pnpm for frontend development
- Haskell Language Server (HLS)
- Pre-commit hooks (fourmolu, hlint, prettier, eslint, nixfmt)

Stack is configured to use `system-ghc: true` to use the GHC from Nix.

## Common Commands

### Backend (Haskell)

```bash
# Build
stack build

# Run tests
stack test

# Run API server
stack run

# Format code
fourmolu --mode inplace <file>.hs

# Lint code
hlint <file>.hs
```

### Frontend (TypeScript/Svelte)

```bash
cd frontend

# Install dependencies
pnpm install

# Development server
pnpm dev

# Build for production
pnpm build

# Run tests
pnpm test

# Lint
pnpm lint

# Format
pnpm format
```

## Project Structure

**CRITICAL**: This project uses **package by feature**, NOT package by layer.

**NEVER create modules like**:

- `src/Types/` or `src/Types.hs` - VIOLATION
- `Errors` - VIOLATION
- `Class` - VIOLATION

**ALWAYS organize by feature**:

- `src/BlackScholes/` - Everything for Black-Scholes pricing (types, logic, API
  endpoints, tests)
- `src/MarketData/` - Everything for market data (types, fetching, caching, API
  endpoints)
- `src/Portfolio/` - Everything for portfolio analysis (types, calculations, API
  endpoints)

Each feature module contains ALL related code: types, business logic, API
endpoints, and tests.

**Frontend** (SvelteKit):

- `frontend/src/lib/components/` - UI components organized by feature
- `frontend/src/lib/api/` - API client
- `frontend/src/routes/` - SvelteKit routes
- Feature-specific code lives together

**Legacy**: `OptionsPricing.hs` contains old exploration code and will be
refactored and moved elsewhere.

## Architecture Principles

### Backend

**CRITICAL: Package by Feature, NOT by Layer**

Organizing by layer (Types/, Models/, API/) is a MAJOR VIOLATION of our
guidelines.

**Type-Driven Development** (within features):

- Create feature module (e.g., `src/BlackScholes/`)
- Define types in the feature module
- Implement business logic in the same module
- Add API endpoints in the same module
- Co-locate tests with implementation

**Servant API Design**:

- Type-level API specification
- Automatic JSON serialization via `aeson`
- Type-safe request/response handling

**Functional Style**:

- Pure functions for pricing models
- No mutable state
- Explicit error handling (no `unwrap`)
- Use `Protolude` for modern Haskell

**Extensive Test Coverage**:

- `hspec` for basic scenario tests (cover happy paths and error cases)
- `QuickCheck` for property tests
- Add mocks for external services when needed

### Frontend

**Svelte 5 Patterns**:

- Use runes for reactivity (`$state`, `$derived`, `$effect`)
- Keep components focused and composable
- Co-locate related code (types, logic, UI)

**Type Safety**:

- Generate TypeScript types from backend API
- Strict TypeScript mode enabled
- Validate API responses

**Component Organization**:

- Package by feature, not by layer
- Each feature owns its types, API, components, and tests

**Test Coverage**:

- Test coverage for frontend code is also expected
- Frontend test coverage can be less thorough as all mission-critical
  computations should be done on the backend

## Key Concepts

### Financial Domain

**Options Pricing**:

- Black-Scholes model for European options
- Greeks (Delta, Gamma, Vega, Theta, Rho)
- Time to expiry, volatility, risk-free rate

**Crypto-Specific**:

- 24/7 trading (365 days/year vs 252 for traditional)
- High volatility environments
- Unique market microstructure

### Type System

Important Haskell types (see [SPEC.md](SPEC.md) for full definitions):

- `OptionType`: Call | Put
- `OptionStyle`: European | American
- `BlackScholesInput`: Pricing parameters
- `OptionPrice`: Price with Greeks
- `Greeks`: Delta, Gamma, Vega, Theta, Rho

### GHC Configuration

The project uses strict GHC settings:

- `-Wall -Werror`: All warnings are errors
- Many language extensions enabled by default (see `package.yaml`)
- `NoImplicitPrelude`: Uses Protolude instead of base Prelude

Key extensions:

- `OverloadedStrings`, `RecordWildCards`, `BlockArguments`
- `LambdaCase`, `TypeApplications`
- See `package.yaml` for complete list

## Development Workflow

### Adding a New Feature

1. **Read documentation**: Review [SPEC.md](SPEC.md) and
   [ROADMAP.md](ROADMAP.md)
2. **Backend first**: Define types → implement model → add API endpoint → write
   tests
3. **Frontend next**: Create components → integrate with API → add tests
4. **Format and lint**: Run pre-commit hooks before committing

### Code Quality Standards

**Never**:

- Use `#[allow(clippy::*)]` without explicit permission (wrong language but
  principle applies)
- Silently truncate or cap financial values
- Use `unwrap` or `error` in production code

**Always**:

- Handle errors explicitly
- Validate all inputs (backend AND frontend)
- Write tests for business logic
- Keep visibility as restrictive as possible
- Add comments only when code can't express intent

### Testing Strategy

**Backend** (HSpec):

- Unit tests for pricing models
- Property tests for invariants (call-put parity)
- API integration tests

**Frontend** (Vitest + Testing Library):

- Component tests in isolation
- Integration tests for user flows
- Mock API responses for tests

## Documentation

- **[README.md](README.md)** - Getting started guide
- **[SPEC.md](SPEC.md)** - Technical specification and architecture
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap and phases
- **[CLAUDE.md](CLAUDE.md)** - This file (AI development guidelines)

For architectural decisions, API design, or domain modeling questions, consult
[SPEC.md](SPEC.md) first.
