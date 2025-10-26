<script lang="ts">
  import * as Card from "$lib/components/ui/card"
  import katex from "katex"

  let callFormulaElement: HTMLDivElement | undefined = $state()
  let dPlusFormulaElement: HTMLDivElement | undefined = $state()
  let dMinusFormulaElement: HTMLDivElement | undefined = $state()
  let putFormulaElement: HTMLDivElement | undefined = $state()

  $effect(() => {
    const callFormula = String.raw`C(S_t, t) = N(d_+)S_t - N(d_-)Ke^{-r(T-t)}`
    const dPlusFormula = String.raw`d_+ = \frac{1}{\sigma\sqrt{T-t}}` +
      String.raw`\left[\ln\left(\frac{S_t}{K}\right) + ` +
      String.raw`\left(r + \frac{\sigma^2}{2}\right)(T-t)\right]`
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
