// Root ESLint config that delegates to frontend config
export default [
  {
    ignores: [
      "**/node_modules/**",
      "**/.svelte-kit/**",
      "**/build/**",
      "**/dist/**",
      // Ignore all root-level files except frontend directory
      "*.hs",
      "*.cabal",
      "*.yaml",
      ".stack-work/**",
    ],
  },
];
