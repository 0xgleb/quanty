<script lang="ts">
  import { Button } from "$lib/components/ui/button"
  import * as Card from "$lib/components/ui/card"
  import { getHealth, getPlaceholder } from "$lib/api/effectClient"
  import { createQueryFx, createMutationFx } from "$lib/query"

  const healthQuery = createQueryFx(["health"] as const, getHealth)
  const placeholderMutation = createMutationFx(getPlaceholder)

  const healthVersion = $derived(healthQuery.data?.version ?? null)
  const backendAvailable = $derived(healthQuery.isSuccess)
</script>

<svelte:head>
  <title>Quanty - Options Pricing Calculator</title>
  <meta
    name="description"
    content="Cryptocurrency options pricing and financial derivatives calculator"
  />
</svelte:head>

<div class="container mx-auto px-4 py-8 max-w-4xl">
  <div class="mb-8">
    <h1 class="text-4xl font-bold mb-2">Quanty</h1>
    <p class="text-muted-foreground text-lg">
      Options Pricing Calculator for Cryptocurrency Derivatives
    </p>
  </div>

  <div class="mb-6 flex items-center gap-2">
    <div
      class="w-3 h-3 rounded-full {healthQuery.isSuccess
        ? 'bg-green-500'
        : healthQuery.isError
          ? 'bg-red-500'
          : 'bg-yellow-500 animate-pulse'}"
    ></div>
    <span class="text-sm text-muted-foreground">
      {#if healthQuery.isPending}
        Checking backend status...
      {:else if healthQuery.isSuccess}
        Backend connected (v{healthVersion || "unknown"})
      {:else}
        Backend unavailable
      {/if}
    </span>
  </div>

  <div class="space-y-6">
    <Card.Root>
      <Card.Header>
        <Card.Title>Test API Connection</Card.Title>
        <Card.Description>
          Fetch placeholder data from the backend to verify end-to-end
          connectivity
        </Card.Description>
      </Card.Header>
      <Card.Content class="space-y-4">
        <Button
          onclick={() => placeholderMutation.mutate()}
          disabled={placeholderMutation.isPending || !backendAvailable}
        >
          {#if placeholderMutation.isPending}
            Loading...
          {:else}
            Fetch Placeholder Data
          {/if}
        </Button>

        {#if placeholderMutation.isError}
          <div
            class="p-4 rounded-lg bg-destructive/10 text-destructive text-sm"
          >
            Error: {placeholderMutation.error?.message ?? "Failed to fetch data"}
          </div>
        {/if}

        {#if placeholderMutation.isSuccess && placeholderMutation.data}
          <div class="p-4 rounded-lg bg-muted space-y-2">
            <div class="font-medium">Response:</div>
            <div class="text-sm">
              <div><strong>Message:</strong> {placeholderMutation.data.message}</div>
              <div><strong>Timestamp:</strong> {placeholderMutation.data.timestamp}</div>
            </div>
          </div>
        {/if}
      </Card.Content>
    </Card.Root>

    <Card.Root>
      <Card.Header>
        <Card.Title>Getting Started</Card.Title>
      </Card.Header>
      <Card.Content>
        <p class="text-sm text-muted-foreground mb-4">
          This is a placeholder page demonstrating the connection between the
          Haskell backend and SvelteKit frontend.
        </p>
        <ul class="text-sm text-muted-foreground space-y-2 list-disc list-inside">
          <li>Backend API running on http://localhost:8080</li>
          <li>Frontend dev server running on http://localhost:5173</li>
          <li>API client auto-generated from OpenAPI specification</li>
          <li>Full type safety from backend to frontend</li>
        </ul>
      </Card.Content>
    </Card.Root>
  </div>
</div>
