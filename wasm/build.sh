#!/usr/bin/env bash
set -euxo pipefail

cd "$(dirname "$0")"

mkdir -p dist

wasm32-wasi-cabal configure \
  --enable-library-vanilla \
  --builddir=dist-wasm

wasm32-wasi-cabal build --builddir=dist-wasm

cp "$(wasm32-wasi-cabal list-bin exe:quanty-wasm --builddir=dist-wasm)" dist/quanty.wasm

# Get the post-linker path
POST_LINK="$(wasm32-wasi-ghc --print-libdir)/post-link.mjs"

echo "Generating JavaScript FFI glue..."
node "$POST_LINK" \
  --input dist/quanty.wasm \
  --output dist/ghc_wasm_jsffi.js

echo "Generating TypeScript types..."
wasmtime --dir=.. dist-wasm/build/wasm32-wasi/ghc-9.12.2.20250924/quanty-wasm-0.1.0.0/x/gen-types/build/gen-types/gen-types.wasm

echo "✓ Build complete!"
echo "  WASM binary: wasm/dist/quanty.wasm"
echo "  FFI glue:    wasm/dist/ghc_wasm_jsffi.js"
echo "  TS types:    frontend/src/lib/wasm/types.ts"

ls -lh dist/quanty.wasm dist/ghc_wasm_jsffi.js
