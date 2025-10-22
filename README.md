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

## License

BSD3
