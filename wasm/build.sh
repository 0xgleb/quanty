#!/usr/bin/env bash
set -euo pipefail

# Build script for Haskell WASM module
# Compiles Haskell to WebAssembly and generates JavaScript FFI glue code

echo "Building Haskell WASM module..."

# Navigate to wasm directory
cd "$(dirname "$0")"

# Create dist directory if it doesn't exist
mkdir -p dist

# Configure Cabal for WASM target
echo "Configuring Cabal..."
wasm32-wasi-cabal configure \
  --enable-library-vanilla \
  --builddir=dist-wasm

# Build the WASM executable
echo "Compiling Haskell to WASM..."
wasm32-wasi-cabal build --builddir=dist-wasm

# Copy WASM binary to dist/
echo "Copying WASM binary..."
cp "$(wasm32-wasi-cabal list-bin exe:quanty-wasm --builddir=dist-wasm)" dist/quanty.wasm

# Get the post-linker path
POST_LINK="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"

# Generate JavaScript FFI glue code
echo "Generating JavaScript FFI glue..."
node "$POST_LINK" \
  --input dist/quanty.wasm \
  --output dist/ghc_wasm_jsffi.js

echo "✓ Build complete!"
echo "  WASM binary: wasm/dist/quanty.wasm"
echo "  FFI glue:    wasm/dist/ghc_wasm_jsffi.js"

# Display file sizes
echo ""
echo "File sizes:"
ls -lh dist/quanty.wasm dist/ghc_wasm_jsffi.js
