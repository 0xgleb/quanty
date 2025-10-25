<script lang="ts">
  import { Button } from "$lib/components/ui/button"
  import * as Card from "$lib/components/ui/card"
  import { Input } from "$lib/components/ui/input"
  import { Label } from "$lib/components/ui/label"
  import { createMutation } from "$lib/query.svelte"
  import {
    BlackScholesService,
    BlackScholesServiceLive,
    getErrorMessage,
  } from "$lib/services/blackScholes"
  import { Effect } from "effect"
  import type { Inputs, OptionKind } from "$lib/api/generated/types.gen"
  import katex from "katex"
  import { formatNumber } from "$lib/formatNumber"
  import { presets, type Preset } from "$lib/presets"

  let callFormulaElement: HTMLDivElement | undefined = $state()
  let dPlusFormulaElement: HTMLDivElement | undefined = $state()
  let dMinusFormulaElement: HTMLDivElement | undefined = $state()
  let putFormulaElement: HTMLDivElement | undefined = $state()

  type FormData = {
    spot: number | undefined
    strike: number | undefined
    timeToExpiry: { days: number | undefined }
    volatility: number | undefined
    riskFreeRate: number | undefined
  }

  let formData = $state<FormData>({
    spot: undefined,
    strike: undefined,
    timeToExpiry: { days: undefined },
    volatility: undefined,
    riskFreeRate: undefined,
  })

  let optionKind = $state<OptionKind>("Call")

  const calculateMutation = createMutation(() =>
    Effect.gen(function* () {
      if (
        formData.spot === undefined ||
        formData.strike === undefined ||
        formData.timeToExpiry.days === undefined ||
        formData.volatility === undefined ||
        formData.riskFreeRate === undefined
      ) {
        return yield* Effect.fail(
          new Error("All fields must be filled"),
        )
      }

      const inputs: Inputs = {
        spot: formData.spot,
        strike: formData.strike,
        timeToExpiry: { days: formData.timeToExpiry.days },
        volatility: formData.volatility,
        riskFreeRate: formData.riskFreeRate,
        kind: optionKind,
      }

      const service = yield* BlackScholesService
      return yield* service.calculatePrice(inputs)
    }).pipe(
      Effect.provide(BlackScholesServiceLive),
      Effect.catchAll((error) =>
        Effect.fail(
          error instanceof Error
            ? error
            : new Error("Unexpected error occurred"),
        ),
      ),
    ),
  )

  const handleSubmit = (event: Event) => {
    event.preventDefault()
    calculateMutation.mutate(undefined)
  }

  const handleReset = () => {
    formData = {
      spot: undefined,
      strike: undefined,
      timeToExpiry: { days: undefined },
      volatility: undefined,
      riskFreeRate: undefined,
    }
    optionKind = "Call"
    calculateMutation.reset()
  }

  const loadPreset = (preset: Preset) => {
    formData = {
      spot: preset.values.spot,
      strike: preset.values.strike,
      timeToExpiry: { days: preset.values.timeToExpiry.days },
      volatility: preset.values.volatility,
      riskFreeRate: preset.values.riskFreeRate,
    }
    optionKind = preset.values.kind
    calculateMutation.mutate(undefined)
  }

  $effect(() => {
    const callFormula = String.raw`C(S_t, t) = N(d_+)S_t - N(d_-)Ke^{-r(T-t)}`
    const dPlusFormula = String.raw`d_+ = \frac{1}{\sigma\sqrt{T-t}}\left[\ln\left(\frac{S_t}{K}\right) + \left(r + \frac{\sigma^2}{2}\right)(T-t)\right]`
    const dMinusFormula = String.raw`d_- = d_+ - \sigma\sqrt{T-t}`
    const putFormula = String.raw`P(S_t, t) = N(-d_-)Ke^{-r(T-t)} - N(-d_+)S_t`

    const renderOptions = {
      displayMode: true,
      throwOnError: false,
      maxSize: 8,
    }

    try {
      if (callFormulaElement)
        katex.render(callFormula, callFormulaElement, renderOptions)
      if (dPlusFormulaElement)
        katex.render(dPlusFormula, dPlusFormulaElement, renderOptions)
      if (dMinusFormulaElement)
        katex.render(dMinusFormula, dMinusFormulaElement, renderOptions)
      if (putFormulaElement)
        katex.render(putFormula, putFormulaElement, renderOptions)
    } catch (error) {
      console.error("KaTeX rendering error:", error)
    }
  })
</script>

<svelte:head>
  <title>Black-Scholes Calculator - Quanty</title>
  <meta
    name="description"
    content="Calculate option prices using the Black-Scholes model for cryptocurrency derivatives"
  />
  <link
    rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/katex@0.16.25/dist/katex.min.css"
    crossorigin="anonymous"
  />
</svelte:head>

<div class="container mx-auto px-6 py-6">
  <div class="mb-6">
    <h1 class="text-3xl font-bold mb-1">Black-Scholes Calculator</h1>
    <p class="text-muted-foreground text-sm">
      Price European options using the Black-Scholes model
    </p>
  </div>

  <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
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
                  onclick={() => loadPreset(preset)}
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
          <form onsubmit={handleSubmit} class="space-y-5">
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
                onclick={handleReset}
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

      <div class="space-y-6">
        <Card.Root>
          <Card.Header>
            <Card.Title>Black-Scholes Formula</Card.Title>
            <Card.Description>
              European option pricing
            </Card.Description>
          </Card.Header>
          <Card.Content>
            <div class="space-y-6">
              <div>
                <p class="text-xs font-medium text-muted-foreground mb-2">Call Option:</p>
                <div bind:this={callFormulaElement} class="text-center"></div>
              </div>

              <div>
                <p class="text-xs font-medium text-muted-foreground mb-2">where:</p>
                <div bind:this={dPlusFormulaElement} class="text-center"></div>
              </div>

              <div>
                <div bind:this={dMinusFormulaElement} class="text-center"></div>
              </div>

              <div>
                <p class="text-xs font-medium text-muted-foreground mb-2">Put Option:</p>
                <div bind:this={putFormulaElement} class="text-center"></div>
              </div>

              <div class="text-xs text-muted-foreground space-y-1 pt-2 border-t">
                <p><em>S<sub>t</sub></em> = Spot price</p>
                <p><em>K</em> = Strike price</p>
                <p><em>T-t</em> = Time to expiry</p>
                <p><em>r</em> = Risk-free rate</p>
                <p><em>σ</em> = Volatility</p>
                <p><em>N(·)</em> = CDF of standard normal</p>
              </div>
            </div>
          </Card.Content>
        </Card.Root>

        {#if calculateMutation.isSuccess && calculateMutation.data}
          <Card.Root>
            <Card.Header>
              <Card.Title>Results</Card.Title>
              <Card.Description>
                Option price and Greeks for your contract
              </Card.Description>
            </Card.Header>
            <Card.Content>
              <div class="space-y-6">
                <div>
                  <div class="text-sm text-muted-foreground mb-1">Option Price</div>
                  <div class="text-3xl font-bold">
                    ${formatNumber(calculateMutation.data.price, 4)}
                  </div>
                  <p class="text-xs text-muted-foreground mt-1">
                    Fair value of the {optionKind.toLowerCase()} option
                  </p>
                </div>

                <div class="space-y-4 pt-4 border-t">
                  <h3 class="text-sm font-medium">Greeks</h3>
                  <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div class="space-y-1">
                      <div class="flex items-baseline gap-2">
                        <span class="text-sm text-muted-foreground">Delta (Δ)</span>
                        <span class="text-lg font-medium">
                          {formatNumber(calculateMutation.data.greeks.delta, 4)}
                        </span>
                      </div>
                      <p class="text-xs text-muted-foreground">
                        Rate of change of option price per $1 change in underlying
                      </p>
                    </div>

                    <div class="space-y-1">
                      <div class="flex items-baseline gap-2">
                        <span class="text-sm text-muted-foreground">Gamma (Γ)</span>
                        <span class="text-lg font-medium">
                          {formatNumber(calculateMutation.data.greeks.gamma, 6)}
                        </span>
                      </div>
                      <p class="text-xs text-muted-foreground">
                        Rate of change of delta per $1 change in underlying
                      </p>
                    </div>

                    <div class="space-y-1">
                      <div class="flex items-baseline gap-2">
                        <span class="text-sm text-muted-foreground">Vega (ν)</span>
                        <span class="text-lg font-medium">
                          {formatNumber(calculateMutation.data.greeks.vega, 4)}
                        </span>
                      </div>
                      <p class="text-xs text-muted-foreground">
                        Change in option price per 1% change in volatility
                      </p>
                    </div>

                    <div class="space-y-1">
                      <div class="flex items-baseline gap-2">
                        <span class="text-sm text-muted-foreground">Theta (Θ)</span>
                        <span class="text-lg font-medium">
                          {formatNumber(calculateMutation.data.greeks.theta, 4)}
                        </span>
                      </div>
                      <p class="text-xs text-muted-foreground">
                        Daily time decay - option value lost per day
                      </p>
                    </div>

                    <div class="space-y-1">
                      <div class="flex items-baseline gap-2">
                        <span class="text-sm text-muted-foreground">Rho (ρ)</span>
                        <span class="text-lg font-medium">
                          {formatNumber(calculateMutation.data.greeks.rho, 4)}
                        </span>
                      </div>
                      <p class="text-xs text-muted-foreground">
                        Change in option price per 1% change in interest rate
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            </Card.Content>
          </Card.Root>
        {/if}
      </div>
  </div>
</div>
