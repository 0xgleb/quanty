<script lang="ts">
  import Card from '$lib/components/ui/card.svelte';
  import { onMount } from 'svelte';

  let health = $state<{ status: string; message: string } | null>(null);
  let info = $state<{ name: string; version: string; description: string } | null>(null);
  let loading = $state(true);
  let error = $state<string | null>(null);

  onMount(async () => {
    try {
      const [healthRes, infoRes] = await Promise.all([fetch('/health'), fetch('/api/info')]);

      if (healthRes.ok && infoRes.ok) {
        health = await healthRes.json();
        info = await infoRes.json();
      } else {
        error = 'Failed to fetch data from API';
      }
    } catch (e) {
      error = e instanceof Error ? e.message : 'Unknown error occurred';
    } finally {
      loading = false;
    }
  });
</script>

<div
  class="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800 p-8"
>
  <div class="max-w-4xl mx-auto space-y-8">
    <div class="text-center space-y-4">
      <h1 class="text-5xl font-bold text-slate-900 dark:text-slate-50">Quanty</h1>
      <p class="text-xl text-slate-600 dark:text-slate-400">Quantitative Finance Platform</p>
    </div>

    <div class="grid md:grid-cols-2 gap-6">
      <Card class="p-6 space-y-4">
        <h2 class="text-2xl font-semibold text-slate-800 dark:text-slate-200">Server Health</h2>
        {#if loading}
          <div class="flex items-center justify-center py-8">
            <div
              class="animate-spin rounded-full h-8 w-8 border-b-2 border-slate-900 dark:border-slate-50"
            ></div>
          </div>
        {:else if error}
          <div class="text-red-600 dark:text-red-400 p-4 bg-red-50 dark:bg-red-900/20 rounded-md">
            {error}
          </div>
        {:else if health}
          <div class="space-y-2">
            <div class="flex items-center gap-2">
              <span class="inline-block w-3 h-3 rounded-full bg-green-500"></span>
              <span class="text-lg font-medium">Status: {health.status}</span>
            </div>
            <p class="text-slate-600 dark:text-slate-400">{health.message}</p>
          </div>
        {/if}
      </Card>

      <Card class="p-6 space-y-4">
        <h2 class="text-2xl font-semibold text-slate-800 dark:text-slate-200">API Information</h2>
        {#if loading}
          <div class="flex items-center justify-center py-8">
            <div
              class="animate-spin rounded-full h-8 w-8 border-b-2 border-slate-900 dark:border-slate-50"
            ></div>
          </div>
        {:else if error}
          <div class="text-red-600 dark:text-red-400 p-4 bg-red-50 dark:bg-red-900/20 rounded-md">
            {error}
          </div>
        {:else if info}
          <div class="space-y-3">
            <div>
              <span class="font-medium text-slate-700 dark:text-slate-300">Name:</span>
              <span class="ml-2 text-slate-600 dark:text-slate-400">{info.name}</span>
            </div>
            <div>
              <span class="font-medium text-slate-700 dark:text-slate-300">Version:</span>
              <span class="ml-2 text-slate-600 dark:text-slate-400">{info.version}</span>
            </div>
            <div>
              <span class="font-medium text-slate-700 dark:text-slate-300">Description:</span>
              <p class="mt-1 text-slate-600 dark:text-slate-400">{info.description}</p>
            </div>
          </div>
        {/if}
      </Card>
    </div>

    <Card class="p-8">
      <div class="space-y-4">
        <h2 class="text-2xl font-semibold text-slate-800 dark:text-slate-200">Getting Started</h2>
        <p class="text-slate-600 dark:text-slate-400">
          This is a placeholder application built with:
        </p>
        <ul class="list-disc list-inside space-y-2 text-slate-600 dark:text-slate-400 ml-4">
          <li><strong>Backend:</strong> Servant (Haskell web framework)</li>
          <li><strong>Frontend:</strong> SvelteKit with shadcn-svelte</li>
          <li><strong>Styling:</strong> Tailwind CSS</li>
        </ul>
        <div class="mt-6 p-4 bg-slate-100 dark:bg-slate-800 rounded-md">
          <p class="text-sm text-slate-600 dark:text-slate-400">
            The backend server is running on <code
              class="px-2 py-1 bg-slate-200 dark:bg-slate-700 rounded">http://localhost:8080</code
            >
          </p>
          <p class="text-sm text-slate-600 dark:text-slate-400 mt-2">Available endpoints:</p>
          <ul class="text-sm text-slate-600 dark:text-slate-400 ml-4 mt-2 space-y-1">
            <li>
              <code class="px-2 py-1 bg-slate-200 dark:bg-slate-700 rounded">GET /health</code> - Health
              check
            </li>
            <li>
              <code class="px-2 py-1 bg-slate-200 dark:bg-slate-700 rounded">GET /api/info</code> - API
              information
            </li>
          </ul>
        </div>
      </div>
    </Card>
  </div>
</div>
