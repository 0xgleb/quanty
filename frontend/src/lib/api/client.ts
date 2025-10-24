import { client } from "./generated/client.gen"

client.setConfig({
  baseUrl: import.meta.env.VITE_API_URL ?? "http://localhost:8080",
})

export { client }
export * from "./generated"
