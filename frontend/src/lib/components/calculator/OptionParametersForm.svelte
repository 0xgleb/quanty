<script lang="ts">
  import { Button } from "$lib/components/ui/button"
  import * as Card from "$lib/components/ui/card"
  import { Input } from "$lib/components/ui/input"
  import { Label } from "$lib/components/ui/label"
  import { getErrorMessage } from "$lib/services/blackScholes"
  import type { OptionKind } from "$lib/api/generated/types.gen"
  import type { Preset } from "$lib/presets"

  type FormData = {
    spot: number | undefined
    strike: number | undefined
    timeToExpiry: { days: number | undefined }
    volatility: number | undefined
    riskFreeRate: number | undefined
  }

  type CalculateMutation = {
    isPending: boolean
    isError: boolean
    error: Error | null
    mutate: () => void
    reset: () => void
  }

  type Props = {
    formData: FormData
    optionKind: OptionKind
    calculateMutation: CalculateMutation
    presets: Preset[]
    onSubmit: (_: Event) => void
    onReset: () => void
    onLoadPreset: (_: Preset) => void
  }

  let {
    formData = $bindable(),
    optionKind = $bindable(),
    calculateMutation,
    presets,
    onSubmit,
    onReset,
    onLoadPreset,
  }: Props = $props()
</script>

<Card.Root>
  <Card.Header>
    <Card.Title>Option Parameters</Card.Title>
    <Card.Description>
      Enter the parameters for your option contract
    </Card.Description>
  </Card.Header>
  <Card.Content>
    <div class="mb-5 pb-5 border-b">
      <div class="text-sm font-medium mb-3">Quick Presets</div>
      <div class="flex flex-wrap gap-2">
        {#each presets as preset}
          <Button
            type="button"
            variant="outline"
            onclick={() => onLoadPreset(preset)}
            disabled={calculateMutation.isPending}
            class="h-auto px-4 py-3 text-xs"
          >
            <div class="flex flex-col items-start gap-0.5">
              <span class="font-medium">{preset.name}</span>
              <span class="text-muted-foreground font-normal">
                {preset.description}
              </span>
            </div>
          </Button>
        {/each}
      </div>
    </div>
    <form onsubmit={onSubmit} class="space-y-5">
      <div class="space-y-1.5">
        <Label>Option Type</Label>
        <div class="inline-flex rounded-lg border border-input overflow-hidden" role="group">
          <label
            class="flex items-center px-3 py-1.5 text-sm font-medium cursor-pointer transition-colors border-r border-input first:rounded-l-lg last:rounded-r-lg {optionKind === 'Call' ? 'bg-primary text-primary-foreground' : 'bg-background hover:bg-muted'}"
          >
            <input
              type="radio"
              name="optionKind"
              value="Call"
              bind:group={optionKind}
              class="sr-only"
            />
            Call
          </label>
          <label
            class="flex items-center px-3 py-1.5 text-sm font-medium cursor-pointer transition-colors first:rounded-l-lg last:rounded-r-lg {optionKind === 'Put' ? 'bg-primary text-primary-foreground' : 'bg-background hover:bg-muted'}"
          >
            <input
              type="radio"
              name="optionKind"
              value="Put"
              bind:group={optionKind}
              class="sr-only"
            />
            Put
          </label>
        </div>
        <p class="text-xs text-muted-foreground">
          Call: Right to buy | Put: Right to sell
        </p>
      </div>

      <div class="space-y-1.5">
        <Label for="spot">
          Spot Price
          <span class="text-muted-foreground font-normal">(S)</span>
        </Label>
        <Input
          id="spot"
          type="number"
          step="0.01"
          placeholder="e.g., 111000 (current BTC ~$111k)"
          bind:value={formData.spot}
          required
        />
        <p class="text-xs text-muted-foreground">
          Current market price of the underlying asset. Check exchange prices (Binance, Coinbase, etc.) or use CoinGecko/CoinMarketCap for spot rates.
        </p>
      </div>

      <div class="space-y-1.5">
        <Label for="strike">
          Strike Price
          <span class="text-muted-foreground font-normal">(K)</span>
        </Label>
        <Input
          id="strike"
          type="number"
          step="0.01"
          placeholder="e.g., 115000 (ATM 111k, OTM 115k)"
          bind:value={formData.strike}
          required
        />
        <p class="text-xs text-muted-foreground">
          The agreed-upon price at which you can buy (call) or sell (put) the asset. Common strikes are at-the-money (ATM = spot price), or 5-10% above/below spot for out-of/in-the-money options.
        </p>
      </div>

      <div class="space-y-1.5">
        <Label for="timeToExpiry">
          Time to Expiry
          <span class="text-muted-foreground font-normal">(T, in days)</span>
        </Label>
        <Input
          id="timeToExpiry"
          type="number"
          step="1"
          placeholder="e.g., 30 for monthly, 7 for weekly"
          bind:value={formData.timeToExpiry.days}
          required
        />
        <p class="text-xs text-muted-foreground">
          Days until option expiration. Common terms: 7 (weekly), 30 (monthly), 90 (quarterly). Calculate from today to expiry date. Longer duration = higher option value (more time for price movement).
        </p>
      </div>

      <div class="space-y-1.5">
        <Label for="volatility">
          Volatility
          <span class="text-muted-foreground font-normal">(σ, annualized)</span>
        </Label>
        <Input
          id="volatility"
          type="number"
          step="0.001"
          placeholder="e.g., 0.45 (current BTC IV ~45%)"
          bind:value={formData.volatility}
          required
        />
        <p class="text-xs text-muted-foreground">
          Annual price volatility as a decimal (e.g., 0.80 = 80%). Use historical volatility calculators or implied volatility from option markets. Crypto: typically 0.50-1.20 (50-120%). Higher volatility = higher option prices.
        </p>
      </div>

      <div class="space-y-1.5">
        <Label for="riskFreeRate">
          Risk-Free Rate
          <span class="text-muted-foreground font-normal">(r, annualized)</span>
        </Label>
        <Input
          id="riskFreeRate"
          type="number"
          step="0.001"
          placeholder="e.g., 0.039 (current 3M T-bill ~3.9%)"
          bind:value={formData.riskFreeRate}
          required
        />
        <p class="text-xs text-muted-foreground">
          Annual risk-free interest rate as a decimal (e.g., 0.05 = 5%). Use current US Treasury yields (check FRED or Treasury.gov for 3-month T-bill rates), or stablecoin lending rates for crypto. Typical range: 0.03-0.06 (3-6%).
        </p>
      </div>

      <div class="flex gap-3 pt-2">
        <Button
          type="submit"
          disabled={calculateMutation.isPending}
          class="flex-1"
        >
          {#if calculateMutation.isPending}
            Calculating...
          {:else}
            Calculate Price
          {/if}
        </Button>
        <Button
          type="button"
          variant="outline"
          onclick={onReset}
          disabled={calculateMutation.isPending}
        >
          Reset
        </Button>
      </div>

      {#if calculateMutation.isError}
        {@const errorDetails = getErrorMessage(calculateMutation.error)}
        <div
          class="p-4 rounded-lg bg-destructive/10 text-destructive text-sm"
        >
          <div class="font-medium mb-1">{errorDetails.title}</div>
          <div>{errorDetails.message}</div>
        </div>
      {/if}
    </form>
  </Card.Content>
</Card.Root>
