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
7. Add OpenAPI generation to backend (servant-openapi3)
8. Generate TypeScript client from OpenAPI spec (@hey-api/openapi-ts)
9. Build placeholder frontend page that calls the backend and displays the
   response
10. Update documentation and verify the full implementation
11. Final cleanup and validation

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

- [x] Create `src/Api.hs` with basic Servant API type definition
- [x] Define `PlaceholderResponse` data type with JSON serialization
- [x] Implement `GET /placeholder` endpoint handler
- [x] Add health check endpoint `GET /health`
- [x] Update `package.yaml` to include new modules and dependencies
- [x] Create `app/Main.hs` to run the Servant server
- [x] Build and test the API endpoints locally with `curl`
- [x] Verify JSON response format

**Expected Output**:

```json
GET /health
{ "status": "ok", "version": "0.1.0" }

GET /placeholder
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

- [x] Update `package.yaml` to add test dependencies:
  - `hspec` (test framework)
  - `hspec-wai` (WAI application testing)
  - `hspec-wai-json` (JSON response helpers)
  - `aeson` (already a dependency, needed for JSON)
- [x] Create `test/Spec.hs` with hspec-discover preprocessor directive:
  ```haskell
  {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
  ```
- [x] Create test directory structure mirroring `src/`:
  ```
  test/
  ├── Spec.hs
  └── ApiSpec.hs
  ```
- [x] Create `test/ApiSpec.hs` with initial tests for placeholder endpoints
- [x] Write test specs for `GET /health` endpoint:
  - Should return 200 status
  - Should return JSON with "status" and "version" fields
  - Should return `status: "ok"`
- [x] Write test specs for `GET /placeholder` endpoint:
  - Should return 200 status
  - Should return JSON with "message" and "timestamp" fields
  - Should return expected message text
- [x] Run tests: `stack test --fast`
- [x] Verify all tests pass
- [x] Add test section to README.md explaining how to run tests

**Expected Test Structure**:

```haskell
module ApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with (return app) $ do
  describe "GET /health" $ do
    it "responds with 200" $ do
      get "/health" `shouldRespondWith` 200

    it "returns valid health status" $ do
      get "/health" `shouldRespondWith`
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

- [x] Create root `package.json` with workspace configuration:
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
- [x] Create `pnpm-workspace.yaml` at project root:
  ```yaml
  packages:
    - "frontend"
  ```
- [x] Verify workspace setup with `pnpm -r list` (should show frontend
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

- [x] Run `pnpm dlx sv create frontend` in project root (choose TypeScript,
      SvelteKit demo app template)
- [x] Verify the `frontend/` directory structure is created correctly
- [x] Run `pnpm install` from frontend directory to install dependencies
- [x] Add TailwindCSS: `pnpm dlx sv add tailwindcss`
- [x] Verify Tailwind configuration files are created (`@import 'tailwindcss'`
      in `app.css`, `@tailwindcss/vite` plugin in `vite.config.ts`)
- [x] Check that `svelte.config.js` has default `$lib` alias configured
- [x] Test that basic SvelteKit app runs: `pnpm dev`
- [x] Update `frontend/.gitignore` if needed

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

- [x] Add prettier configuration to `frontend/package.json`:
  ```json
  "prettier": {
    "tabWidth": 2,
    "semi": false,
    "quoteProps": "consistent",
    "arrowParens": "avoid"
  }
  ```
- [x] Install ESLint dependencies:
      `pnpm add -D eslint @typescript-eslint/eslint-plugin @typescript-eslint/parser eslint-plugin-svelte svelte-eslint-parser @eslint/js globals`
- [x] Create `frontend/eslint.config.js` with TypeScript + Svelte configuration
      (ESLint v9 flat config)
- [x] Create root `eslint.config.js` to satisfy pre-commit hooks
- [x] Update `frontend/package.json` scripts:
  ```json
  {
    "lint": "eslint .",
    "format": "prettier --write ."
  }
  ```
- [x] Update `frontend/tsconfig.json` to enable strict mode and unused variable
      warnings (`noUnusedLocals`, `noUnusedParameters`)
- [x] Test that `pnpm lint` and `pnpm format` work correctly
- [x] Verify pre-commit hooks work for frontend files
- [x] Configure `flake.nix` to exclude TypeScript files from denofmt (to avoid
      conflicts with prettier)

**Note**: The root `flake.nix` already has prettier/eslint hooks enabled, so
they should automatically apply to frontend files.

---

## Task 6. Initialize shadcn-svelte

Set up shadcn-svelte component library and add initial components.

**Rationale**: shadcn-svelte provides accessible, customizable components that
integrate well with TailwindCSS. The CLI handles most configuration
automatically.

### Subtasks

- [x] Run `pnpm dlx shadcn-svelte@latest init` from `frontend/` directory
- [x] Select configuration during CLI prompts:
  - Base color: Zinc (selected during init)
  - CSS file location: default (`src/app.css`)
  - Import aliases: default (`$lib` paths)
- [x] Verify `components.json` is created with correct configuration
- [x] Verify `src/lib/utils.ts` is created with `cn()` helper
- [x] Add first component: `pnpm dlx shadcn-svelte@latest add button`
- [x] Add card component: `pnpm dlx shadcn-svelte@latest add card`
- [x] Verify components are created in `src/lib/components/ui/`
- [x] Test importing and using Button component (verified via dev server
      startup)

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

## Task 7. Add OpenAPI Generation to Backend

Generate OpenAPI 3.0 specification from Servant API using `servant-openapi3`.

**Rationale**: Instead of manually writing TypeScript types, we leverage
Servant's type-level API to generate an OpenAPI spec. This spec becomes the
single source of truth for API types and enables automatic TypeScript client
generation (Task 8). This approach:

- Eliminates manual type definitions and synchronization bugs
- Provides documented API spec useful for testing tools and Swagger UI
- Follows industry-standard OpenAPI ecosystem
- More maintainable than direct Servant→TypeScript libraries (servant-typescript
  has minimal maintenance)
- Uses separate executable for generation (no need to run server)

### Subtasks

- [x] Add dependencies to `package.yaml`:
  ```yaml
  dependencies:
    - servant-openapi3
    - openapi3
    - bytestring
    - lens
  ```
- [x] Run `stack build` to install new dependencies
- [x] Update `src/Api.hs` imports:
  ```haskell
  import Control.Lens ((.~), (?~))
  import Data.OpenApi (ToSchema)
  import Data.OpenApi qualified as OpenApi
  import Servant.OpenApi qualified
  ```
- [x] Derive `ToSchema` instances for response types:

  ```haskell
  data HealthResponse = HealthResponse
    { status :: Text
    , version :: Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)

  data PlaceholderResponse = PlaceholderResponse
    { message :: Text
    , timestamp :: Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, ToSchema)
  ```

- [x] Create `apiOpenApi` function in `src/Api.hs`:
  ```haskell
  apiOpenApi :: OpenApi.OpenApi
  apiOpenApi =
    Servant.OpenApi.toOpenApi (Proxy :: Proxy (NamedRoutes API))
      & OpenApi.info . OpenApi.title .~ "Quanty API"
      & OpenApi.info . OpenApi.version .~ "0.1.0"
      & OpenApi.info . OpenApi.description
          ?~ "Options pricing and financial derivatives API"
  ```
- [x] Export `apiOpenApi` from `src/Api.hs`
- [x] Create `app/generate-openapi/Main.hs` executable:

  ```haskell
  module Main (main) where

  import Protolude
  import Data.Aeson qualified as Aeson
  import Data.ByteString.Lazy qualified as LBS
  import Api (apiOpenApi)

  main :: IO ()
  main = do
    let openApiJson = Aeson.encode apiOpenApi
    LBS.writeFile "openapi.json" openApiJson
    putStrLn ("Generated openapi.json" :: Text)
  ```

- [x] Add executable to `package.yaml`:

  ```yaml
  executables:
    quanty-exe:
    # ... existing Main.hs config

    generate-openapi:
      main: Main.hs
      source-dirs: app/generate-openapi
      dependencies:
        - quanty
  ```

- [x] Build and run generator:
  ```bash
  stack build --fast
  stack exec generate-openapi
  ```
- [x] Verify `openapi.json` is created at project root
- [x] Verify JSON contains correct paths, schemas, and types:
  ```bash
  jq . openapi.json
  ```
- [x] Add `openapi.json` to git (it's a build artifact but should be versioned)
- [x] Optionally: Add `/openapi.json` endpoint to serve spec at runtime (useful
      for Swagger UI later) - SKIPPED
- [x] Add comment to `src/Api.hs` explaining regeneration:
  ```haskell
  -- | Generate OpenAPI spec by running: stack exec generate-openapi
  -- This creates openapi.json at project root for TypeScript client generation.
  apiOpenApi :: OpenApi.OpenApi
  ```

**Expected OpenAPI Output Structure**:

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "Quanty API",
    "version": "0.1.0"
  },
  "paths": {
    "/health": { ... },
    "/placeholder": { ... }
  },
  "components": {
    "schemas": {
      "HealthResponse": { ... },
      "PlaceholderResponse": { ... }
    }
  }
}
```

**When to Regenerate**:

- After changing API types or adding/removing endpoints
- Command: `stack exec generate-openapi`
- Commit the updated `openapi.json`

---

## Task 8. Generate TypeScript Client from OpenAPI Spec

Use `@hey-api/openapi-ts` to generate fully-typed TypeScript client from the
OpenAPI specification file.

**Rationale**: The `openapi.json` file from Task 7 enables automatic generation
of TypeScript types and API client functions. `@hey-api/openapi-ts` is the
actively-maintained modern tool for this purpose (replaces deprecated
openapi-typescript-codegen). Reading from file is simpler than hitting a running
server.

### Subtasks

- [x] Install client generator in frontend:
  ```bash
  cd frontend
  pnpm add -D @hey-api/openapi-ts
  ```
- [x] Create `frontend/openapi-ts.config.ts`:

  ```typescript
  import { defineConfig } from "@hey-api/openapi-ts";

  export default defineConfig({
    input: "../openapi.json",
    output: "src/lib/api/generated",
    client: "fetch",
  });
  ```

- [x] Add npm script to `frontend/package.json`:
  ```json
  {
    "scripts": {
      "generate-client": "openapi-ts"
    }
  }
  ```
- [x] Generate TypeScript client:
  ```bash
  cd frontend
  pnpm generate-client
  ```
- [x] Verify generated files in `frontend/src/lib/api/generated/`:
  - `types.gen.ts` - TypeScript types for all API models
  - `sdk.gen.ts` - API client functions
  - `client/` and `core/` - Internal client implementation
- [x] Create wrapper `frontend/src/lib/api/client.ts`:

  ```typescript
  import { client } from "./generated/client.gen";

  // Configure base URL
  client.setConfig({
    baseUrl: import.meta.env.VITE_API_URL ?? "http://localhost:8080",
  });

  export { client };
  export * from "./generated";
  ```

- [x] Add generated files to `.gitignore` (artifacts should not be versioned)
- [x] Test importing in a `.svelte` file to verify types work
- [x] Update `README.md` with client generation workflow:
  - Full command:
    `stack exec generate-openapi && cd frontend && pnpm generate-client`
  - When to regenerate: after changing Haskell API types

**Expected Generated Client Usage**:

```typescript
import { DefaultService } from "$lib/api/client";

// Fully typed API calls
const health = await DefaultService.getApiV1Health();
// health: HealthResponse = { status: string, version: string }

const data = await DefaultService.getApiV1Placeholder();
// data: PlaceholderResponse = { message: string, timestamp: string }
```

**Full Regeneration Workflow**:

1. Make changes to Haskell API types in `src/Api.hs`
2. Run `stack exec generate-openapi` to update `openapi.json`
3. Run `cd frontend && pnpm generate-client` to update TypeScript client
4. Commit both `openapi.json` and generated TypeScript files

---

## Task 9. Build Placeholder Frontend Page

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
- [ ] Add health check indicator that calls `/health` on mount
- [ ] Test the full flow: click button → API call → display response

**Expected UI**:

- Clean, minimal page with title "Quanty - Options Pricing Calculator"
- Health status indicator (green if backend is healthy)
- Button: "Fetch Placeholder Data"
- Card displaying API response when available
- Loading spinner during request
- Error message if backend is unreachable

---

## Task 10. Documentation and Testing

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

## Task 11. Final Cleanup and Validation

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

1. ✅ Haskell backend serves `/health` and `/placeholder` endpoints
2. ✅ Hspec testing infrastructure is set up with auto-discovery
3. ✅ Backend tests pass with `stack test --fast` (all endpoint tests green)
4. ✅ Test directory structure mirrors `src/` structure
5. ✅ SvelteKit frontend runs on `http://localhost:5173`
6. ✅ Frontend successfully calls backend API and displays responses
7. ✅ shadcn-svelte components (Button, Card) work correctly
8. ✅ Prettier and ESLint are configured and working
9. ✅ Pre-commit hooks run on both Haskell and TypeScript files
10. ✅ Documentation is updated with frontend setup instructions and testing
    commands
11. ✅ Both `stack build --fast`, `stack test --fast`, and `pnpm build` succeed
    without errors
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
  - All commands run from root: `stack build --fast`, `stack test --fast`,
    `pnpm dev`, `pnpm build`
- Package-by-feature organization will be enforced starting in Phase 1
  (Black-Scholes module)
- For now, `src/Api.hs` can contain the simple placeholder endpoint
- TypeScript types and API client are auto-generated from Servant API via
  OpenAPI 3.0 spec (`servant-openapi3` → `@hey-api/openapi-ts`)
- Generated code is committed to git (both `openapi.json` and TypeScript client)
- CORS configuration may be needed if frontend/backend run on different ports
  (will handle when testing)
