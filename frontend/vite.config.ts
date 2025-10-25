import tailwindcss from "@tailwindcss/vite"
import { sveltekit } from "@sveltejs/kit/vite"
import { svelteTesting } from "@testing-library/svelte/vite"
import { defineConfig } from "vitest/config"

export default defineConfig(({ mode }) => ({
  plugins: [tailwindcss(), sveltekit(), svelteTesting()],
  resolve: {
    conditions: mode === "test" ? ["browser"] : [],
  },
  define: {
    "process.env": {},
  },
  build: {
    target: "es2023",
  },
  test: {
    globals: true,
    environment: "jsdom",
    include: [
      "src/**/*.{test,spec}.{js,ts}",
      "src/**/*.svelte.{test,spec}.{js,ts}",
    ],
    exclude: ["src/lib/api/generated/**"],
    setupFiles: ["./test-setup.ts"],
    testTimeout: 10000,
    alias: {
      $lib: "/src/lib",
    },
    server: {
      deps: {
        inline: [/bits-ui/, /katex/],
      },
    },
  },
}))
