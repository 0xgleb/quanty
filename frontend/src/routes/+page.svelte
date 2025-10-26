<script lang="ts">
  import { createMutation } from "$lib/query.svelte"
  import {
    BlackScholesService,
    BlackScholesServiceLive,
  } from "$lib/services/blackScholes"
  import { Effect } from "effect"
  import type { Inputs, OptionKind } from "$lib/api/generated/types.gen"
  import { presets, type Preset } from "$lib/presets"
  import OptionParametersForm from "$lib/components/calculator/OptionParametersForm.svelte"
  import FormulaDisplay from "$lib/components/calculator/FormulaDisplay.svelte"
  import ResultsDisplay from "$lib/components/calculator/ResultsDisplay.svelte"

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
    }).pipe(Effect.provide(BlackScholesServiceLive)),
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
    integrity="sha384-ZlsGkaTGe72LcQRdHcP5S1MYvfltZaG/vkBXb5WhMPAzP1k8vCPOm8h9Pj1aIkC"
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
      <OptionParametersForm
        bind:formData
        bind:optionKind
        {calculateMutation}
        {presets}
        onSubmit={handleSubmit}
        onReset={handleReset}
        onLoadPreset={loadPreset}
      />

      <div class="space-y-6">
        <FormulaDisplay />

        {#if calculateMutation.isSuccess && calculateMutation.data}
          <ResultsDisplay data={calculateMutation.data} {optionKind} />
        {/if}
      </div>
  </div>
</div>
