# Plan: Setup GitHub Actions CI

This plan sets up continuous integration using GitHub Actions with the same Nix environment used in development.

## Task 1. Create GitHub Actions workflow configuration

Create `.github/workflows/ci.yml` that:
- Runs on push to all branches and pull requests
- Uses Nix to ensure reproducible builds matching dev environment
- Runs all pre-commit hooks and build/test checks
- Has separate jobs for better parallelization and clear failure reporting

**Subtasks:**
- [x] Set up Nix with cachix/install-nix-action
- [x] Configure Nix cache for faster builds
- [x] Create job for Nix-level checks (nix flake check)
- [x] Create job for backend checks (Haskell)
- [x] Create job for frontend checks (TypeScript/Svelte)

## Task 2. Backend CI checks

Configure backend job to run all Haskell checks:
- [x] Format check with fourmolu
- [x] Lint with hlint
- [x] Build with stack build --fast
- [x] Test with stack test --fast

## Task 3. Frontend CI checks

Configure frontend job to run all TypeScript/Svelte checks:
- [x] Install dependencies with pnpm
- [x] Lint with eslint
- [x] Format check with prettier
- [x] Type check with svelte-check
- [x] Run tests with vitest
- [x] Build check

## Task 4. Nix and misc checks

Configure checks for Nix files and shell scripts:
- [x] Format check with nixfmt
- [x] Nix flake check
- [x] Shellcheck for any shell scripts

## Design Decisions

**Why separate jobs?**
- Parallel execution for faster feedback
- Clear failure reporting (know immediately if it's backend vs frontend)
- Can run on different runners if needed in the future

**Why use Nix in CI?**
- Ensures exact same environment as development
- Reproducible builds
- Caching works well with Nix store
- No version drift between dev and CI

**Why install-nix-action?**
- Official way to use Nix in GitHub Actions
- Handles Nix installation and configuration
- Works well with GitHub Actions cache

**Cache strategy:**
- Use cachix for Nix store caching (already configured in flake.nix)
- Use GitHub Actions cache for additional speedup
- Cache stack and pnpm dependencies separately

**Check order:**
- Fast checks first (formatting, linting) to fail fast
- Build checks next
- Test checks last (usually slowest)
