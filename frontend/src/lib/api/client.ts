// API client wrapper with configured base URL
import { client } from "./generated/client.gen"

// Configure base URL from environment variable or default to localhost
client.setConfig({
  baseUrl: import.meta.env.VITE_API_URL ?? "http://localhost:8080",
})

// Re-export everything from generated client
export { client }
export * from "./generated"
