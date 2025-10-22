# Quanty

A quantitative finance platform built with Servant (Haskell) for the backend and SvelteKit with shadcn-svelte for the frontend.

## Project Structure

```
quanty/
├── app/              # Haskell executable (server entry point)
├── src/              # Haskell library code (API definitions)
├── frontend/         # SvelteKit frontend application
│   └── src/
│       ├── lib/      # Reusable components and utilities
│       └── routes/   # SvelteKit routes
├── package.yaml      # Haskell package configuration
└── flake.nix         # Nix development environment
```

## Backend (Servant)

### Dependencies

The backend uses:
- **Servant**: Type-safe web API framework
- **Warp**: High-performance web server
- **Aeson**: JSON encoding/decoding

### Available Endpoints

- `GET /health` - Health check endpoint
- `GET /api/info` - API information

### Running the Backend

Using Stack:
```bash
stack build
stack exec quanty-server
```

The server will start on `http://localhost:8080`

## Frontend (SvelteKit + shadcn-svelte)

### Dependencies

The frontend uses:
- **SvelteKit**: Web framework for Svelte
- **shadcn-svelte**: Component library based on shadcn/ui
- **Tailwind CSS**: Utility-first CSS framework

### Running the Frontend

```bash
cd frontend
npm install
npm run dev
```

The frontend will start on `http://localhost:5173` with a proxy to the backend API.

## Development

### Full Stack Development

1. Start the backend server:
   ```bash
   stack exec quanty-server
   ```

2. In a separate terminal, start the frontend:
   ```bash
   cd frontend
   npm run dev
   ```

3. Open your browser to `http://localhost:5173`

The frontend is configured to proxy API requests to the backend, so both `/health` and `/api/info` endpoints will work seamlessly.

## Building for Production

### Backend
```bash
stack build --copy-bins
```

### Frontend
```bash
cd frontend
npm run build
npm run preview
```

## Development Workflow

### Git Hooks

This project uses pre-commit hooks to ensure code quality. The hooks are automatically installed when you enter the Nix development shell:

```bash
nix develop
```

The following hooks are configured:
- **fourmolu**: Format Haskell code
- **hlint**: Lint Haskell code
- **nixpkgs-fmt**: Format Nix files
- **prettier**: Format frontend code (JavaScript, TypeScript, Svelte)
- **trailing-whitespace**: Remove trailing whitespace
- **end-of-file-fixer**: Ensure files end with a newline

### Makefile Commands

The project includes a Makefile with helpful commands:

```bash
make help              # Show available commands
make install-hooks     # Install git pre-commit hooks
make format            # Auto-format all code
make check-format      # Check formatting without modifying files
make lint              # Run linters
make test              # Run all tests
make build             # Build both backend and frontend
make check             # Run all checks (format, lint, test)
make clean             # Clean build artifacts
make dev-backend       # Start backend development server
make dev-frontend      # Start frontend development server
```

### CI/CD

This project uses GitHub Actions for continuous integration. All CI jobs use Nix to manage dependencies consistently.

The following checks run on every push and pull request:

1. **Nix Flake Checks**: Validate the entire Nix flake configuration
2. **Build and Test**: Build Haskell backend, run tests, and build frontend
3. **Format Check**: Verify code formatting (Fourmolu for Haskell, Prettier for frontend, nixpkgs-fmt for Nix)
4. **Lint**: Run HLint on Haskell code

All tools and dependencies are provided by the Nix development environment.

### Running Checks Locally

Before pushing your changes, you can run all checks locally:

```bash
# Using Makefile
make check

# Or using Nix flakes
nix flake check
```

## License

BSD3
