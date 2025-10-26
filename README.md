# Quanty

A web application for pricing cryptocurrency options and other financial
derivatives. Built with Haskell and TypeScript (with Effect) for
high-performance quantitative finance calculations with end-to-end type safety
and robust error handling.

## Overview

Quanty provides an intuitive interface for pricing crypto options using
industry-standard models. Start with simple manual parameter input and basic
Black-Scholes pricing, then expand to support multiple pricing models, exotic
options, real-time market data, and advanced analytics.

- **[SPEC.md](SPEC.md)** - Technical specification and architecture
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap
- **[AGENTS.md](AGENTS.md)** - AI development assistant instructions

See [SPEC.md](SPEC.md) for architecture details and [ROADMAP.md](ROADMAP.md) for
the development plan.

## Features

### Black-Scholes Calculator

Price European options using the classic Black-Scholes model:

- **Interactive calculator** with real-time results
- **Full Greeks calculation** (Delta, Gamma, Vega, Theta, Rho)
- **Quick presets** for common scenarios (ATM Call, OTM Call, ITM Put)
- **Input validation** with helpful error messages
- **Educational tooltips** explaining each parameter and Greek
- **Responsive design** works on desktop and mobile

**API Endpoint**: `POST /black-scholes`

```bash
curl -X POST http://localhost:8080/black-scholes \
  -H "Content-Type: application/json" \
  -d '{
    "spot": 111000,
    "strike": 115000,
    "timeToExpiry": { "days": 30 },
    "volatility": 0.45,
    "riskFreeRate": 0.039,
    "kind": "Call"
  }'
```

## Getting Started

### Prerequisites

- [Nix](https://github.com/DeterminateSystems/nix-installer) DeterminateSystems
  Nix (recommended)

### Development Setup

1. **Clone the repository**:

   ```bash
   git clone git@github.com:0xgleb/quanty.git
   cd quanty
   ```

2. **Enter development environment**:

   ```bash
   direnv allow  # or: nix develop --impure
   ```

   This provides GHC, Stack, Node.js, pnpm, and all necessary tools.

3. **Install dependencies**:

   ```bash
   stack build --fast            # Backend dependencies
   cd frontend && pnpm install   # Frontend dependencies
   ```

4. **Run the development servers**:

   ```bash
   # Terminal 1: Backend API server
   stack run quanty-exe

   # Terminal 2: Frontend dev server
   cd frontend && pnpm dev
   ```

5. **Access the application**: Open
   [http://localhost:5173](http://localhost:5173) in your browser.

## Development Commands

### Backend (Haskell)

```bash
stack build --fast       # Build the project
stack test --fast        # Run tests
stack exec quanty-exe    # Start API server (after building)

fourmolu --mode inplace <file>.hs  # Format code
hlint <file>.hs          # Lint code
```

**Tip**: Use `--fast` flag during development for faster feedback:

- `stack build --fast` and `stack test --fast` skip optimizations
- Use optimized builds only for production

**Testing**: The project uses `hspec` with automatic test discovery via
`hspec-discover`:

- Tests live in `test/` directory
- Test files must end with `Spec.hs` and export a `spec :: Spec` binding
- API endpoint tests use `hspec-wai` for testing Servant applications
- Run tests with `stack test --fast` during development

### Frontend (TypeScript/Svelte)

All frontend commands must be run from the `frontend/` directory:

```bash
cd frontend

pnpm dev                 # Start dev server with HMR
pnpm build               # Build for production
pnpm preview             # Preview production build
pnpm test                # Run tests
pnpm lint                # Lint code
pnpm format              # Format code
pnpm generate-client     # Generate TypeScript API client from OpenAPI spec
```

### API Client Generation

The frontend TypeScript API client is automatically generated from the Haskell
backend's OpenAPI specification. This ensures type-safe end-to-end communication
between frontend and backend.

**When to regenerate the client**:

- After changing API types in Haskell
- After adding or removing API endpoints
- After modifying request/response structures

**Full regeneration workflow**:

```bash
# 1. Generate OpenAPI spec from Haskell API
stack exec generate-openapi

# 2. Generate TypeScript client from OpenAPI spec
cd frontend && pnpm generate-client
```

**What gets generated**:

- `frontend/src/lib/api/generated/types.gen.ts` - TypeScript types for all API
  models
- `frontend/src/lib/api/generated/sdk.gen.ts` - Fully-typed API client functions
- `frontend/src/lib/api/client.ts` - Configured client wrapper (manual, not
  generated)

**Usage in components**:

```typescript
import { getHealth, getPlaceholder } from "$lib/api/client";
import type { HealthResponse, PlaceholderResponse } from "$lib/api/client";

// Fully typed API calls
const health = await getHealth(); // health: HealthResponse
const data = await getPlaceholder(); // data: PlaceholderResponse
```

### Code Quality

Pre-commit hooks (configured in `flake.nix`) automatically run on staged files:

- **fourmolu**: Haskell formatting
- **hlint**: Haskell linting
- **prettier**: Frontend formatting
- **eslint**: Frontend linting
- **nixfmt**: Nix file formatting

**Manual usage**:

```bash
pre-commit run -a           # Run all hooks on all files
pre-commit run fourmolu     # Run specific hook
pre-commit run --files src/Api.hs  # Run on specific file
```

Hooks run automatically on `git commit`. To bypass (not recommended):

```bash
git commit --no-verify
```
