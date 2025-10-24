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

### Key Features

- **Options Pricing**: Black-Scholes model with support for crypto-specific
  adjustments
- **Interactive UI**: Modern web interface built with Svelte 5 and shadcn
  components
- **Type-Safe Error Handling**: Effect-based architecture for robust error
  handling and async operations
- **Real-Time Calculations**: Fast Haskell backend with Servant API
- **Runtime Validation**: Effect Schema for type-safe data validation
- **Multiple Instruments**: Support for various option types and derivatives
  (planned)
- **Market Data Integration**: Real-time and historical crypto price feeds
  (planned)
- **Visualization**: Interactive charts and Greeks analysis (planned)

## Tech Stack

### Backend

- **Haskell**: Core computation engine and API server
- **Servant**: Type-safe REST API framework
- **Protolude**: Modern Prelude replacement

### Frontend

- **SvelteKit**: Full-stack framework with SSR support
- **Svelte 5**: Reactive UI framework with runes
- **TypeScript**: Type-safe frontend development
- **Effect**: Type-safe error handling and async operations
- **@effect/schema**: Runtime validation and type generation
- **shadcn/ui**: Component library for consistent design
- **TailwindCSS**: Utility-first styling

### Infrastructure

- **Nix**: Reproducible development environment and dependency management
- **Stack**: Haskell build tool configured to use Nix GHC

## Getting Started

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

### Development Setup

1. **Clone the repository**:

   ```bash
   git clone <repository-url>
   cd quanty
   ```

2. **Enter development environment**:

   ```bash
   direnv allow  # or: nix develop --impure
   ```

   This provides GHC, Stack, Node.js, pnpm, and all necessary tools.

3. **Build the backend**:

   ```bash
   stack build
   ```

4. **Install frontend dependencies**:

   ```bash
   cd frontend
   pnpm install
   ```

5. **Run the development servers**:

   ```bash
   # Terminal 1: Backend API server
   stack run

   # Terminal 2: Frontend dev server
   cd frontend
   pnpm dev
   ```

6. **Access the application**: Open
   [http://localhost:5173](http://localhost:5173) in your browser.

## Development Commands

### Backend (Haskell)

```bash
stack build              # Build the project
stack test               # Run tests
stack run                # Start API server

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

## Documentation

- **[SPEC.md](SPEC.md)** - Technical specification and architecture
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap
- **[CLAUDE.md](CLAUDE.md)** - AI development assistant instructions

See [SPEC.md](SPEC.md) for architecture details and [ROADMAP.md](ROADMAP.md) for
the development plan.
