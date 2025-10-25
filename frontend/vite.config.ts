import tailwindcss from "@tailwindcss/vite"
import { sveltekit } from "@sveltejs/kit/vite"
import { defineConfig } from "vitest/config"

export default defineConfig(({ mode }) => ({
  plugins: [tailwindcss(), sveltekit()],
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
    include: ["src/**/*.{test,spec}.{js,ts}"],
    exclude: ["src/lib/__mocks__/**/*.ts", "src/lib/api/generated/**"],
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
