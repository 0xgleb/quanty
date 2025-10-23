# Implementation Plan: Frontend Initialization with shadcn-svelte

This plan implements [issue #2](https://github.com/0xgleb/quanty/issues/2) -
Initialize frontend with shadcn-svelte.

## Overview

This implementation follows a vertical slice approach:

1. Create a basic Haskell Servant endpoint that returns placeholder data
2. Set up Haskell testing infrastructure with hspec-discover and hspec-wai
3. Set up pnpm workspace to enable running all commands from root
4. Initialize SvelteKit project following shadcn-svelte official installation
   guide
5. Configure TypeScript tooling (prettier, eslint) following patterns from
   metagenda/strike
6. Set up shadcn-svelte with TailwindCSS
7. Create TypeScript API client for backend communication
8. Build placeholder frontend page that calls the backend and displays the
   response
9. Update documentation and verify the full implementation
10. Final cleanup and validation

**Key Design Decisions**:

- Use `pnpm` as package manager (already in Nix environment)
- **Use pnpm workspaces**: Root `package.json` with workspace configuration
  allows running all commands from root
- Frontend code lives in `frontend/` but commands run from root via workspace
  scripts
- Follow shadcn-svelte installation guide exactly for initial setup
- Use ESLint legacy config (`.eslintrc.cjs`) initially for simplicity
- Keep prettier config in `package.json` to match metagenda/strike patterns
- Create placeholder endpoint in `src/Api.hs` (temporary location until we have
  feature modules)

---

## Task 1. Create Placeholder Backend Endpoint

Create a minimal Servant API endpoint that returns JSON data for the frontend to
consume.

**Rationale**: Having a working backend endpoint first ensures we can test
end-to-end connectivity once the frontend is ready. This validates our API
design early.

### Subtasks

- [ ] Create `src/Api.hs` with basic Servant API type definition
- [ ] Define `PlaceholderResponse` data type with JSON serialization
- [ ] Implement `GET /api/v1/placeholder` endpoint handler
- [ ] Add health check endpoint `GET /api/v1/health`
- [ ] Update `package.yaml` to include new modules and dependencies
- [ ] Create `app/Main.hs` to run the Servant server
- [ ] Build and test the API endpoints locally with `curl`
- [ ] Verify JSON response format

**Expected Output**:

```json
GET /api/v1/health
{ "status": "ok", "version": "0.1.0" }

GET /api/v1/placeholder
{ "message": "Hello from Quanty API", "timestamp": "2025-10-22T..." }
```

---

## Task 2. Set Up Haskell Testing Infrastructure

Set up hspec with auto-discovery and hspec-wai for testing Servant endpoints.

**Rationale**: Establishing testing infrastructure early ensures we can write
tests alongside implementation. hspec-discover automatically finds and runs all
test modules, and hspec-wai provides helpers for testing WAI/Servant
applications without running a full server.

### Subtasks

- [ ] Update `package.yaml` to add test dependencies:
  - `hspec` (test framework)
  - `hspec-wai` (WAI application testing)
  - `hspec-wai-json` (JSON response helpers)
  - `aeson` (already a dependency, needed for JSON)
- [ ] Create `test/Spec.hs` with hspec-discover preprocessor directive:
  ```haskell
  {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
  ```
- [ ] Create test directory structure mirroring `src/`:
  ```
  test/
  ├── Spec.hs
  └── ApiSpec.hs
  ```
- [ ] Create `test/ApiSpec.hs` with initial tests for placeholder endpoints
- [ ] Write test specs for `GET /api/v1/health` endpoint:
  - Should return 200 status
  - Should return JSON with "status" and "version" fields
  - Should return `status: "ok"`
- [ ] Write test specs for `GET /api/v1/placeholder` endpoint:
  - Should return 200 status
  - Should return JSON with "message" and "timestamp" fields
  - Should return expected message text
- [ ] Run tests: `stack test`
- [ ] Verify all tests pass
- [ ] Add test section to README.md explaining how to run tests

**Expected Test Structure**:

```haskell
module ApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with (return app) $ do
  describe "GET /api/v1/health" $ do
    it "responds with 200" $ do
      get "/api/v1/health" `shouldRespondWith` 200

    it "returns valid health status" $ do
      get "/api/v1/health" `shouldRespondWith`
        [json|{status: "ok", version: "0.1.0"}|]
```

**Design Notes**:

- hspec-discover requires test modules to export a `spec :: Spec` binding
- Test file names must end with `Spec.hs`
- Test structure should mirror `src/` structure (e.g., `src/Api.hs` →
  `test/ApiSpec.hs`)
- As we add more modules in future phases, add corresponding test files in the
  same structure

---

## Task 3. Set Up pnpm Workspace

Create root `package.json` with pnpm workspace configuration to enable running
frontend commands from root.

**Rationale**: Using pnpm workspaces allows running both Haskell (`stack`) and
TypeScript (`pnpm`) commands from the project root without switching
directories. This improves developer experience and makes automation easier.

### Subtasks

- [ ] Create root `package.json` with workspace configuration:
  ```json
  {
    "name": "quanty",
    "version": "0.1.0",
    "private": true,
    "scripts": {
      "dev": "pnpm --filter frontend dev",
      "build": "pnpm --filter frontend build",
      "preview": "pnpm --filter frontend preview",
      "lint": "pnpm --filter frontend lint",
      "format": "pnpm --filter frontend format"
    },
    "pnpm": {
      "overrides": {}
    }
  }
  ```
- [ ] Create `pnpm-workspace.yaml` at project root:
  ```yaml
  packages:
    - "frontend"
  ```
- [ ] Verify workspace setup with `pnpm -r list` (should show frontend
      workspace)

**Expected Project Structure After**:

```
quanty/
├── package.json          # Root workspace config
├── pnpm-workspace.yaml   # Workspace definition
├── package.yaml          # Haskell config
├── stack.yaml            # Haskell build
├── src/                  # Haskell source
├── test/                 # Haskell tests
└── frontend/             # Frontend workspace (created in next task)
```

---

## Task 4. Initialize SvelteKit Project

Follow the official shadcn-svelte installation guide to create the SvelteKit
project.

**Rationale**: Following the official guide ensures we get the correct project
structure and dependencies. shadcn-svelte expects specific SvelteKit
conventions.

### Subtasks

- [ ] Run `pnpm dlx sv create frontend` in project root (choose TypeScript,
      SvelteKit demo app template)
- [ ] Verify the `frontend/` directory structure is created correctly
- [ ] Run `pnpm install` from root to install frontend dependencies via
      workspace
- [ ] Add TailwindCSS: `pnpm --filter frontend dlx sv add tailwindcss`
- [ ] Verify Tailwind configuration files are created (`tailwind.config.ts`,
      `app.css`)
- [ ] Check that `svelte.config.js` has default `$lib` alias configured
- [ ] Test that basic SvelteKit app runs from root: `pnpm dev`
- [ ] Update `frontend/.gitignore` if needed

**Expected Project Structure**:

```
frontend/
├── src/
│   ├── routes/
│   │   └── +page.svelte
│   ├── lib/
│   └── app.html
├── static/
├── package.json
├── svelte.config.js
├── vite.config.ts
└── tsconfig.json
```

---

## Task 5. Configure TypeScript Tooling

Set up prettier and eslint following patterns from metagenda/strike projects.

**Rationale**: Consistent code formatting and linting prevents issues and
integrates with existing pre-commit hooks in the Nix environment.

### Subtasks

- [ ] Add prettier configuration to `frontend/package.json`:
  ```json
  "prettier": {
    "tabWidth": 2,
    "semi": false,
    "quoteProps": "consistent",
    "arrowParens": "avoid"
  }
  ```
- [ ] Install ESLint dependencies:
      `pnpm add -D eslint @typescript-eslint/eslint-plugin @typescript-eslint/parser eslint-plugin-svelte svelte-eslint-parser`
- [ ] Create `frontend/.eslintrc.cjs` with TypeScript + Svelte configuration
- [ ] Update `frontend/package.json` scripts:
  ```json
  {
    "lint": "eslint . --ext ts,svelte --report-unused-disable-directives --max-warnings 0",
    "format": "prettier --write ."
  }
  ```
- [ ] Update `frontend/tsconfig.json` to enable strict mode and unused variable
      warnings
- [ ] Test that `pnpm lint` and `pnpm format` work correctly
- [ ] Verify pre-commit hooks work for frontend files (stage a file and commit
      to test)

**Note**: The root `flake.nix` already has prettier/eslint hooks enabled, so
they should automatically apply to frontend files.

---

## Task 6. Initialize shadcn-svelte

Set up shadcn-svelte component library and add initial components.

**Rationale**: shadcn-svelte provides accessible, customizable components that
integrate well with TailwindCSS. The CLI handles most configuration
automatically.

### Subtasks

- [ ] Run `pnpm dlx shadcn-svelte@latest init` from `frontend/` directory
- [ ] Select configuration during CLI prompts:
  - Base color: Slate (neutral, professional)
  - CSS file location: default (`src/app.css`)
  - Import aliases: default (`$lib` paths)
- [ ] Verify `components.json` is created with correct configuration
- [ ] Verify `src/lib/utils.ts` is created with `cn()` helper
- [ ] Add first component: `pnpm dlx shadcn-svelte@latest add button`
- [ ] Add card component: `pnpm dlx shadcn-svelte@latest add card`
- [ ] Verify components are created in `src/lib/components/ui/`
- [ ] Test importing and using Button component in a page

**Expected Structure After**:

```
frontend/src/lib/
├── components/
│   └── ui/
│       ├── button/
│       └── card/
└── utils.ts
```

---

## Task 7. Create API Client

Build a TypeScript API client for communicating with the Haskell backend.

**Rationale**: Centralizing API calls in a client module makes it easier to
maintain, test, and eventually generate types from the Haskell API.

### Subtasks

- [ ] Create `frontend/src/lib/api/client.ts` with base API URL configuration
- [ ] Define TypeScript types matching backend responses:
  ```typescript
  export type HealthResponse = { status: string; version: string };
  export type PlaceholderResponse = { message: string; timestamp: string };
  ```
- [ ] Implement `fetchHealth()` function using native `fetch`
- [ ] Implement `fetchPlaceholder()` function
- [ ] Add error handling for network failures and non-200 responses
- [ ] Add JSDoc comments explaining each function
- [ ] Test API client functions manually in browser console

**Design Note**: For now, use manual type definitions. In future tasks, we'll
generate TypeScript types from Servant API using `servant-typescript` or similar
tools.

---

## Task 8. Build Placeholder Frontend Page

Create a simple home page that calls the backend and displays results using
shadcn components.

**Rationale**: This completes the vertical slice by proving end-to-end
connectivity from frontend to backend. It also validates that shadcn-svelte
components work correctly.

### Subtasks

- [ ] Update `frontend/src/routes/+page.svelte` to create home page
- [ ] Add page title and description
- [ ] Use Svelte 5 runes (`$state`, `$derived`, `$effect`) for reactive state
- [ ] Create button that calls `fetchPlaceholder()` on click
- [ ] Display API response in a shadcn Card component
- [ ] Add loading state while request is in flight
- [ ] Add error state if request fails
- [ ] Style the page using TailwindCSS utilities
- [ ] Add health check indicator that calls `/api/v1/health` on mount
- [ ] Test the full flow: click button → API call → display response

**Expected UI**:

- Clean, minimal page with title "Quanty - Options Pricing Calculator"
- Health status indicator (green if backend is healthy)
- Button: "Fetch Placeholder Data"
- Card displaying API response when available
- Loading spinner during request
- Error message if backend is unreachable

---

## Task 9. Documentation and Testing

Update documentation and verify the implementation.

**Rationale**: Documentation ensures future developers (and AI assistants)
understand the setup. Testing ensures everything works correctly.

### Subtasks

- [ ] Update `README.md` with frontend setup instructions:
  - Explain pnpm workspace setup
  - How to install dependencies (`pnpm install` from root)
  - How to run dev server (`pnpm dev` from root)
  - How to format/lint (`pnpm format`, `pnpm lint` from root)
  - Mention that frontend code lives in `frontend/` but commands run from root
- [ ] Add workspace commands to "Development Commands" section
- [ ] Update `SPEC.md` if needed (API endpoint documentation)
- [ ] Test full development workflow from root:
  - [ ] Start backend: `stack run`
  - [ ] Start frontend: `pnpm dev` (from root, not from frontend/)
  - [ ] Verify health check works
  - [ ] Verify placeholder endpoint works
  - [ ] Verify UI displays data correctly
- [ ] Test that pre-commit hooks work on frontend files
- [ ] Verify that `nix develop` provides all necessary tools for frontend
      development
- [ ] Clean up any unnecessary files from initial SvelteKit template

---

## Task 10. Final Cleanup and Validation

Ensure the implementation is production-ready and follows all guidelines.

**Rationale**: This final pass ensures code quality, removes dead code, and
verifies all tooling works correctly.

### Subtasks

- [ ] Run `pnpm format` from root (formats frontend code via workspace)
- [ ] Run `pnpm lint` and fix any issues
- [ ] Run `fourmolu --mode inplace` on all Haskell files
- [ ] Run `hlint` on all Haskell files and fix suggestions
- [ ] Build backend: `stack build` (verify no warnings)
- [ ] Build frontend: `pnpm build` from root (verify build succeeds)
- [ ] Remove any unused dependencies from `frontend/package.json`
- [ ] Remove any unused imports or dead code
- [ ] Verify all files pass pre-commit hooks
- [ ] Test production build: `pnpm preview` from root
- [ ] Create a git commit (do not push yet - wait for review)

---

## Success Criteria

This implementation is complete when:

1. ✅ Haskell backend serves `/api/v1/health` and `/api/v1/placeholder`
   endpoints
2. ✅ Hspec testing infrastructure is set up with auto-discovery
3. ✅ Backend tests pass with `stack test` (all endpoint tests green)
4. ✅ Test directory structure mirrors `src/` structure
5. ✅ SvelteKit frontend runs on `http://localhost:5173`
6. ✅ Frontend successfully calls backend API and displays responses
7. ✅ shadcn-svelte components (Button, Card) work correctly
8. ✅ Prettier and ESLint are configured and working
9. ✅ Pre-commit hooks run on both Haskell and TypeScript files
10. ✅ Documentation is updated with frontend setup instructions and testing
    commands
11. ✅ Both `stack build`, `stack test`, and `pnpm build` succeed without errors
12. ✅ The implementation follows all guidelines in AGENTS.md (package by
    feature will apply in later phases)

---

## Notes

- This is Phase 0 foundation work - the focus is on getting basic infrastructure
  working
- **Workspace structure**: Both Haskell and TypeScript package files are at root
  - Haskell: `package.yaml`, `stack.yaml` at root
  - TypeScript: Root `package.json` with workspace, `frontend/package.json` for
    frontend
  - All commands run from root: `stack build`, `stack test`, `pnpm dev`,
    `pnpm build`
- Package-by-feature organization will be enforced starting in Phase 1
  (Black-Scholes module)
- For now, `src/Api.hs` can contain the simple placeholder endpoint
- The TypeScript API client types are manual for now; type generation from
  Servant comes later
- CORS configuration may be needed if frontend/backend run on different ports
  (will handle when testing)
