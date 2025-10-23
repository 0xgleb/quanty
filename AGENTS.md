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

## Plan & Review

### Before starting work

- Write a comprehensive step-by-step plan to PLAN.md with each task having a
  corresponding section and a list of subtasks as checkboxes inside of it
- The task sections should follow the format `## Task N. <TASK NAME>`
- The plan should be a detailed implementation plan and the reasoning behind the
  design decisions
- Do not include timelines in the plan as they tend to be inaccurate
- Remain focused on the task at hand, do not include unrelated improvements or
  premature optimizations
- Once you write the plan, ask me to review it. Do not continue until I approve
  the plan.

### While implementing

- **CRITICAL: Complete tasks one at a time and wait for review**
  - When asked to complete a task from a plan, complete ONLY that task
  - Do NOT proceed to the next task until the user reviews and approves your
    changes
  - The user manually reviews all git diffs, so changes must be minimal and
    focused
  - **Any diff not required to complete the task is a guideline violation** - no
    drive-by improvements, refactorings, or style changes unless explicitly
    included in the scope of the task or requested by the user
  - Exception: If the user explicitly asks you to "complete the whole plan" or
    "complete the entire feature", you may work through multiple tasks
  - By default, always work one task at a time
- **CRITICAL: Tasks must be ordered correctly in plans**
  - When creating implementation plans, ensure tasks are in the correct order
  - Earlier tasks MUST NOT depend on code from later tasks
  - All checks (tests, hlint, fourmolu, prettier, eslint) SHOULD pass at the end
    of each task whenever possible
  - Focused git diffs and passing checks make reviewing much easier
- Update PLAN.md every time you complete a task by marking checkboxes as `[x]`
- Keep PLAN.md concise - just tick off checkboxes, do not add "Changes Made"
  sections or verbose changelogs
- The code diffs themselves should be self-explanatory and easy to review

### Before creating a PR

- **CRITICAL**: Delete PLAN.md before submitting changes for review
- PLAN.md is a transient development file that should ONLY exist on development
  branches
- PLAN.md should NEVER appear in pull requests or be merged to main/master
- The plan is for development tracking only - final documentation goes in commit
  messages, docstrings, and permanent markdown documents
- **CRITICAL**: Update all documentation to reflect your changes
  - **ROADMAP.md**: Mark completed tasks as done with the PR link
    - When you complete a task that corresponds to an item in ROADMAP.md, update
      the roadmap to mark it as complete `[x]` and add the PR link
    - Format: `- [x] Task description`
    - Add PR reference: `- **PR:** [#N](pr-url)`
    - This ensures the roadmap accurately reflects progress when the PR is
      merged
  - **README.md**: Review and update if your changes affect:
    - Project structure (new directories, modules)
    - Key features or capabilities
    - Development commands or workflows
    - API endpoints
    - Architecture overview
  - **SPEC.md**: Review and update if your changes affect:
    - Type definitions or data structures
    - API endpoints or request/response formats
    - Pricing models or algorithms
    - Business logic or domain concepts
    - Integration points with external systems
  - **AGENTS.md**: Update if you introduce new patterns, practices, or
    conventions that other developers should follow
  - Out-of-date documentation has negative value - it confuses more than it
    clarifies

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
