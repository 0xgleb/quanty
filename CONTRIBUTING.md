# Contributing to Quanty

Thank you for your interest in contributing to Quanty! This document provides guidelines and instructions for contributing to this project.

## Getting Started

### Prerequisites

- [Nix](https://nixos.org/download.html) (recommended) or:
  - [Stack](https://docs.haskellstack.org/en/stable/) for Haskell development
  - [Node.js](https://nodejs.org/) (v22+) for frontend development
- Git

### Setting Up the Development Environment

#### Using Nix (Recommended)

1. Clone the repository:
   ```bash
   git clone https://github.com/0xgleb/quanty.git
   cd quanty
   ```

2. Enter the Nix development shell:
   ```bash
   nix develop
   ```

   This will:
   - Install all required dependencies
   - Set up git pre-commit hooks automatically
   - Configure the development environment

#### Without Nix

1. Install Stack and Node.js manually
2. Install Haskell tools:
   ```bash
   stack install fourmolu hlint
   ```

3. Install frontend dependencies:
   ```bash
   cd frontend
   npm install
   ```

## Development Workflow

### Making Changes

1. Create a new branch for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes following the code style guidelines

3. Run checks locally before committing:
   ```bash
   make check
   ```

4. Commit your changes (pre-commit hooks will run automatically):
   ```bash
   git add .
   git commit -m "Your descriptive commit message"
   ```

5. Push your changes and create a pull request:
   ```bash
   git push origin feature/your-feature-name
   ```

### Code Style

All code formatting is managed through Nix-provided tools. The pre-commit hooks will automatically format your code.

#### Haskell

- **Fourmolu** for formatting (configuration in `fourmolu.yaml`)
- **HLint** for linting (configuration in `.hlint.yaml`)
- Write meaningful type signatures
- Use explicit module exports
- Add documentation for public functions

#### Frontend (TypeScript/Svelte)

- **Prettier** for formatting (configuration in `frontend/.prettierrc`)
- Follow TypeScript best practices
- Use meaningful variable and function names
- Add JSDoc comments for complex functions

#### Nix

- **nixpkgs-fmt** for formatting
- Keep expressions readable and well-documented

### Running Tests

```bash
# Haskell tests
stack test

# Or using Make
make test
```

### Building the Project

```bash
# Build everything
make build

# Or individually:
stack build          # Backend
cd frontend && npm run build  # Frontend
```

### Running the Application

#### Backend

```bash
make dev-backend
# or
stack exec quanty-server
```

#### Frontend

```bash
make dev-frontend
# or
cd frontend && npm run dev
```

## Pre-commit Hooks

Pre-commit hooks are automatically installed when you enter the Nix development shell. They will:

1. Format your code automatically
2. Run linters
3. Check for common issues

If a hook fails, fix the issues and try committing again.

## CI/CD

All pull requests must pass the following checks (all managed through Nix):

- ✅ Nix flake checks
- ✅ Haskell build and tests
- ✅ Frontend build
- ✅ Code formatting (Fourmolu for Haskell, Prettier for frontend, nixpkgs-fmt for Nix)
- ✅ Haskell linting (HLint)

You can run all checks locally with:

```bash
make check
```

All CI dependencies are managed by Nix, ensuring consistency between local development and CI environments.

## Pull Request Process

1. Ensure your code passes all checks locally
2. Update the README.md or documentation if needed
3. Add tests for new features
4. Write a clear PR description explaining:
   - What changes you made
   - Why you made them
   - How to test them
5. Link any related issues

## Questions?

If you have questions or need help, feel free to:

- Open an issue on GitHub
- Check existing issues and discussions

## License

By contributing to Quanty, you agree that your contributions will be licensed under the BSD3 License.
