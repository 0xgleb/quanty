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

echo "✓ Build complete!"
echo "  WASM binary: wasm/dist/quanty.wasm"
echo "  FFI glue:    wasm/dist/ghc_wasm_jsffi.js"

ls -lh dist/quanty.wasm dist/ghc_wasm_jsffi.js
