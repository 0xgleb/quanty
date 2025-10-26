# AGENTS.md

This file provides guidance to AI agents working with code in this repository.

## Project Overview

Quanty is a web application for pricing cryptocurrency options and financial
derivatives. It combines a Haskell backend for high-performance numerical
computation with a TypeScript/Svelte frontend for modern web UX.

**Key Technologies**:

- **Backend**: Haskell + Servant for REST API
- **Frontend**: SvelteKit + TypeScript + Effect + shadcn/ui
- **Tooling**: Nix flakes for reproducible development environment
- **Language**: Haskell uses Protolude (no implicit Prelude), TypeScript uses
  Effect for type-safe error handling

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

### When to create a plan

Create a plan for feature implementations and multi-step GitHub issues. Do NOT
create a plan when the user asks you to do a change rather than to plan a
change.

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
- **CRITICAL: Always verify checks pass before claiming work is complete**
  - ALWAYS run `stack build --fast` and `stack test --fast` before submitting
    Haskell code for review
  - ALWAYS run `pnpm lint` and `pnpm format` before submitting TypeScript code
    for review
  - NEVER claim that work is complete or ready for review without running all
    relevant checks
  - If checks fail, fix the issues before informing the user
  - The user should never discover failing checks - you must catch them first
- Update PLAN.md every time you complete a task by marking checkboxes as `[x]`
- Keep PLAN.md concise - just tick off checkboxes, do not add "Changes Made"
  sections or verbose changelogs
- The code diffs themselves should be self-explanatory and easy to review

### Before creating a PR

- **CRITICAL**: Delete PLAN.md before submitting changes for review
  - **A PR containing PLAN.md is NOT ready for review**
  - PLAN.md must be removed from the working tree AND from git history (if
    accidentally committed)
  - Use `git rm PLAN.md` to remove it properly
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

**Performance Tip**: Use `--fast` flag during development for faster feedback
loops:

- `stack build --fast` - Skip optimizations for faster builds
- `stack test --fast` - Run tests without optimizations
- Use optimized builds only for production or benchmarking

**Build Responsibility**:

- **User runs build** when dependencies or versions change (monitors download
  progress)
- **AI can run build** for code-only changes (quick, no downloads)

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

**Command Execution Responsibility**:

- **User runs** `pnpm install` when dependencies change (heavy lifting -
  monitors download progress)
- **AI can run** `pnpm build`, `pnpm lint`, `pnpm format`, `pnpm test` for
  code-only changes (quick, no downloads)
- **User runs** indefinite commands like `pnpm dev` (long-running dev servers)

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

- Use **NamedRoutes** pattern for all APIs (modern Servant ergonomics)
- Type-level API specification with explicit named handlers
- Automatic JSON serialization via `aeson`
- Type-safe request/response handling with better error messages

**Functional Style**:

- Pure functions for pricing models
- No mutable state
- Explicit error handling (no `unwrap`)
- Use `Protolude` for modern Haskell

**Naming Conventions**:

- **CRITICAL**: Use types to convey information, not verbose variable names
- Avoid meaningless single-letter names like `d`, `s`, `t`, `x` in business
  logic
- **Bad examples**:
  - `spotPrice :: Double` - information redundantly encoded in name
  - `daysValue :: Double` - information redundantly encoded in name
  - `x :: Price` - name conveys nothing
  - `d :: Days` - name conveys nothing (unless standard notation)
- **Good examples**:
  - `spot :: Price` - type tells you what it is, name is concise
  - `days :: Days` - type tells you what it is, name is concise
  - `err :: SomeError` - standard abbreviation, type is descriptive
  - `tmp :: FilePath` - standard abbreviation, type is descriptive
  - `result :: OptionPrice` - type is descriptive
- Exception: Standard mathematical notation in well-documented formulas (e.g.,
  `d1`, `d2` in Black-Scholes are acceptable when extensively documented)
- The Haskell philosophy: leverage the type system to encode meaning, keep names
  concise but not cryptic

**Import Style**:

- **CRITICAL**: All imports MUST be qualified OR use explicit import lists
- The ONLY exception is `Protolude` which is imported unqualified
- **Operators** (`:>`, `:-`, `<>`, `>>=`, `<$>`, etc.) go in explicit import
  lists
- **Regular names** (types, functions with alphanumeric names) can also go in
  explicit import lists
- Use `ImportQualifiedPost` extension (already enabled in `package.yaml`)
- Example:
  ```haskell
  import Protolude
  import Data.Aeson qualified as Aeson
  import Servant (type (:<|>), type (:>), Get, JSON, Handler, Proxy(..), Server)
  import Servant.Server qualified as Server
  import Network.Wai qualified as Wai
  ```
- **Never** import a whole module unqualified (except `Protolude`)

**Deriving Strategies**:

- **CRITICAL**: Always use explicit deriving strategies
- Prefer `deriving via` for type class instances
- Use `deriving anyclass` for classes with Generic defaults (like ToJSON,
  FromJSON)
- Use `deriving stock` for standard derived instances (Eq, Show, etc.)
- Example:
  ```haskell
  data MyType = MyType { field :: Text }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
  ```

**Language Extensions**:

- **CRITICAL**: DO NOT enable language extensions that are already enabled
  project-wide in `package.yaml` or by **GHC2024**
- See "GHC Configuration" section below for available extensions
- Only add `{-# LANGUAGE #-}` pragmas for extensions specific to a single module
  (rare)

**Modern Record Handling**:

- `DuplicateRecordFields` - Multiple records can have same field names
- `NoFieldSelectors` - No automatic field selector functions
- `OverloadedRecordDot` - Use `record.field` syntax (clean, no name conflicts)
- Example: `user.name` instead of `name user`

**Type-Level Programming with TypeAbstractions**:

- **CRITICAL**: Use `@`-binders instead of `ScopedTypeVariables` for all new
  code
- TypeAbstractions provides principled type variable binding with better
  scoping:
  ```haskell
  id @t x = x :: t  -- binds type variable at use site
  ```
- Pattern matching on existential types:
  ```haskell
  matchType (TList @elem rep) = "List of " ++ matchType rep
  ```
- Works correctly with type synonyms where ScopedTypeVariables fails
- Enables binding type variables under non-injective type constructors
- **Type family arity**: Always specify arity explicitly:
  ```haskell
  type P @k = Proxy @k  -- declares P takes one invisible argument
  ```
- Use `StandaloneKindSignatures` for complex type families

**Advanced Type-Level Features** (available when needed):

- **RequiredTypeArguments** (experimental) - Visible dependent quantification
  using `forall a ->` syntax:
  ```haskell
  sizeOf :: forall a -> Storable a => Int
  -- Call as: sizeOf Bool  (no Proxy needed)
  ```
- **LinearTypes** - Resource safety (`%1 ->` for linear functions)
- **TypeData** - Compile-time-only types for type-level programming
- **ExtendedLiterals** - Direct syntax for sized numerics (`123i8`, `1000w16`)
- **Effectful** - Modern effect system (use when complex effect handling needed)

**Extensive Test Coverage**:

- `hspec` for basic scenario tests (cover happy paths and error cases)
- `QuickCheck` for property tests
- Add mocks for external services when needed

### Frontend

**Effect-Based Architecture**:

- Use Effect for all async operations and error handling
- Model errors explicitly with tagged unions using `Data.TaggedError`
- Define services with Effect for dependency injection
- Use `@effect/schema` (Schema) for runtime validation of external data
- Prefer `Effect.gen` over Promise/async-await for async flows
- Use Layers for composable service configuration
- All API calls return `Effect<Data, Error, Requirements>`
- **Naming convention**: Use `Fx` suffix for Effect-based wrappers (e.g.,
  `createQueryFx`, `createMutationFx`)

**Effect Patterns**:

- API calls: Return `Effect<Data, ApiError, HttpClient>`
- Form validation: Use Schema with custom error messages and refinements
- State management: Integrate Effect with Svelte runes for async state
- Error boundaries: Use `Effect.catchAll` and `Effect.catchTag` for graceful
  error handling
- Retries: Use `Effect.retry` with exponential backoff for transient failures
- Concurrency: Use `Effect.all` for parallel operations, `Effect.race` for
  racing
- Timeouts: Always add `Effect.timeout` to external operations

**Svelte 5 Integration**:

- Use runes for reactivity (`$state`, `$derived`, `$effect`)
- Execute Effect programs with `Effect.runPromise` in components
- Keep components focused and composable
- Co-locate related code (types, schemas, services, UI)

**Type Safety**:

- Generate TypeScript types from backend API
- Define Effect Schemas for all external data (API responses, user input)
- Strict TypeScript mode enabled
- Validate API responses with `Schema.decode` (runtime type checking)
- Use Schema refinements for business rules (e.g., positive numbers, valid
  ranges)
- Define error types explicitly (no `unknown` or `any` without validation)
- End-to-end type safety from API to UI

**Component Organization**:

- Package by feature, not by layer
- Each feature owns its schemas, services, types, API, components, and tests
- Service layer organized in `lib/services/` by feature
- Schemas organized in `lib/schemas/` by domain

**Test Coverage**:

- Test coverage for frontend code is also expected
- Frontend test coverage can be less thorough as all mission-critical
  computations should be done on the backend
- Use Effect test utilities for service testing

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

The project uses **GHC2024** language edition, providing modern type system
features.

**Language Configuration**:

- **Language Edition**: `GHC2024` (configured in `package.yaml`)
- **Warnings**: `-Wall -Werror` (all warnings are errors)
- **Prelude**: `NoImplicitPrelude` (uses Protolude)

**Key GHC2024 Extensions Available**:

- Type system: `GADTs`, `DataKinds`, `TypeApplications`, `RankNTypes`,
  `StandaloneKindSignatures`
- Deriving: `DerivingStrategies`, `DeriveGeneric`, `DeriveAnyClass`
- Syntax: `LambdaCase`, `ImportQualifiedPost`, `ExplicitNamespaces`
- Records: `DisambiguateRecordFields`

**Additional Project Extensions** (see `package.yaml`):

- `DuplicateRecordFields`, `NoFieldSelectors`, `OverloadedRecordDot` - Modern
  record handling
- `TypeFamilyDependencies` - Type family injectivity
- `DerivingVia` - Deriving via newtype coercion
- `OverloadedStrings`, `QuasiQuotes`, `RecordWildCards` - Syntax conveniences

## Development Workflow

### Adding a New Feature

1. **Read documentation**: Review [SPEC.md](SPEC.md) and
   [ROADMAP.md](ROADMAP.md)
2. **Backend first**: Define types → implement model → add API endpoint → write
   tests
3. **Frontend next**: Create components → integrate with API → add tests
4. **Format and lint**: Run pre-commit hooks before committing

### Code Quality Standards

**CRITICAL: Never bypass quality checks without explicit user permission**:

- **NEVER** disable lints with `eslint-disable` comments without permission
- **NEVER** skip or ignore failing tests without permission
- **NEVER** bypass formatters, type checkers, or other quality checks without
  permission
- **NEVER** commit code with known warnings or errors without permission
- If a check fails, fix the issue properly or ask for permission to bypass it

**Never**:

- Use `#[allow(clippy::*)]` without explicit permission (wrong language but
  principle applies)
- Silently truncate or cap financial values
- Use `unwrap` or `error` in production code
- Silently swallow errors (always use `Effect.catchAll` or `Effect.catchTag`)
- Use try/catch in new TypeScript code (use `Effect.try` instead)
- Use `any` or `unknown` without validation (use `Schema.decode`)
- Make external API calls without timeout (use `Effect.timeout`)

**Always**:

- Handle errors explicitly with typed errors
- Use Effect for async operations and error handling in TypeScript
- Define explicit error types with tagged unions
- Validate all external data with Effect Schema
- Use `Effect.gen` for readable async flows
- Add timeout to all external API calls
- Use Layers for dependency injection in TypeScript
- Validate all inputs (backend AND frontend)
- Write tests for business logic
- Keep visibility as restrictive as possible
- Write self-documenting code; avoid unnecessary comments
- Only add comments for complex, unusual, or confusing situations that cannot be
  expressed clearly through code structure, naming, or types

### Testing Strategy

**CRITICAL: Write tests that test YOUR code, not the language**:

- **NEVER** write tests that just verify TypeScript/JavaScript/Haskell
  primitives work
- **NEVER** test that initializing an object with fields results in an object
  with those fields
- **NEVER** test that type annotations work correctly
- Tests should verify YOUR BUSINESS LOGIC, not that the compiler/runtime works
- If a test doesn't catch a potential bug in YOUR code, it's useless
- Example of BAD test: `expect(mockObject.field).toBe(value)` when you just
  initialized it with that value
- Example of GOOD test: Testing that a pricing formula returns expected values
  for known inputs

**Backend** (HSpec):

- Unit tests for pricing models
- Property tests for invariants (call-put parity)
- API integration tests

**Frontend** (Vitest + Testing Library + Effect Test):

- Component tests in isolation
- Integration tests for user flows
- Use `Layer.succeed` to mock Effect services in tests
- Test error scenarios with `Effect.fail`
- Use `Effect.gen` in tests for readable async test code
- Property tests for Schema validation
- Mock API responses with test layers

## Documentation

- **[README.md](README.md)** - Getting started guide
- **[SPEC.md](SPEC.md)** - Technical specification and architecture
- **[ROADMAP.md](ROADMAP.md)** - Development roadmap and phases
- **[CLAUDE.md](CLAUDE.md)** - This file (AI development guidelines)

For architectural decisions, API design, or domain modeling questions, consult
[SPEC.md](SPEC.md) first.
