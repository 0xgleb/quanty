# Quanty

A web application for pricing cryptocurrency options and other financial
derivatives. Built with Haskell and TypeScript for high-performance quantitative
finance calculations.

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
- **Real-Time Calculations**: Fast Haskell backend with Servant API
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

### Frontend (TypeScript/Svelte)

```bash
cd frontend

pnpm dev                 # Start dev server with HMR
pnpm build               # Build for production
pnpm preview             # Preview production build
pnpm test                # Run tests
pnpm lint                # Lint code
pnpm format              # Format code
```

### Code Quality

Pre-commit hooks automatically run on staged files:

- **fourmolu**: Haskell formatting
- **hlint**: Haskell linting
- **prettier**: Frontend formatting
- **eslint**: Frontend linting
- **nixfmt**: Nix file formatting

## Documentation

- **[SPEC.md](SPEC.md)** - Technical specification and architecture
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap
- **[CLAUDE.md](CLAUDE.md)** - AI development assistant instructions

See [SPEC.md](SPEC.md) for architecture details and [ROADMAP.md](ROADMAP.md) for
the development plan.
