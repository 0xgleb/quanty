.PHONY: help format check test build clean install-hooks

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Available targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-20s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install-hooks: ## Install git pre-commit hooks
	@echo "Installing git hooks..."
	@if command -v nix >/dev/null 2>&1; then \
		nix develop --command echo "Git hooks installed!"; \
	else \
		echo "Warning: Nix not installed. Install Nix to use git hooks."; \
	fi

format: ## Format all code (Haskell, Nix, Frontend)
	@echo "Formatting Haskell code..."
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode inplace app/ src/ OptionsPricing.hs; \
	else \
		echo "fourmolu not found, skipping Haskell formatting"; \
	fi
	@echo "Formatting Nix code..."
	@if command -v nixpkgs-fmt >/dev/null 2>&1; then \
		nixpkgs-fmt *.nix; \
	else \
		echo "nixpkgs-fmt not found, skipping Nix formatting"; \
	fi
	@echo "Formatting frontend code..."
	@cd frontend && npm install -D prettier && npx prettier --write "src/**/*.{js,ts,svelte,json,css,html}"

check-format: ## Check code formatting without modifying files
	@echo "Checking Haskell formatting..."
	@if command -v fourmolu >/dev/null 2>&1; then \
		fourmolu --mode check app/ src/ OptionsPricing.hs; \
	else \
		echo "fourmolu not found, skipping Haskell format check"; \
	fi
	@echo "Checking Nix formatting..."
	@if command -v nixpkgs-fmt >/dev/null 2>&1; then \
		nixpkgs-fmt --check *.nix; \
	else \
		echo "nixpkgs-fmt not found, skipping Nix format check"; \
	fi

lint: ## Run linters
	@echo "Running HLint on Haskell code..."
	@if command -v hlint >/dev/null 2>&1; then \
		hlint app/ src/ OptionsPricing.hs; \
	else \
		echo "hlint not found, skipping linting"; \
	fi

test: ## Run all tests
	@echo "Running Haskell tests..."
	@stack test
	@echo "All tests passed!"

build: ## Build the project
	@echo "Building Haskell backend..."
	@stack build
	@echo "Building frontend..."
	@cd frontend && npm install && npm run build
	@echo "Build complete!"

check: check-format lint test ## Run all checks (format, lint, test)
	@echo "All checks passed!"

clean: ## Clean build artifacts
	@echo "Cleaning Haskell build artifacts..."
	@stack clean
	@echo "Cleaning frontend build artifacts..."
	@cd frontend && rm -rf .svelte-kit build node_modules
	@echo "Clean complete!"

dev-backend: ## Start the backend development server
	@echo "Starting backend server..."
	@stack build && stack exec quanty-server

dev-frontend: ## Start the frontend development server
	@echo "Starting frontend dev server..."
	@cd frontend && npm install && npm run dev

dev: ## Start both backend and frontend servers (in separate terminals)
	@echo "Start the backend in one terminal with: make dev-backend"
	@echo "Start the frontend in another terminal with: make dev-frontend"
