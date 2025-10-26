<script lang="ts">
  import * as Card from "$lib/components/ui/card"
  import { formatNumber } from "$lib/formatNumber"
  import type { OptionPrice, OptionKind } from "$lib/api/generated/types.gen"

  let {
    data,
    optionKind,
  }: {
    data: OptionPrice
    optionKind: OptionKind
  } = $props()
</script>

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
          ${formatNumber(data.price, 4)}
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
                {formatNumber(data.greeks.delta, 4)}
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
                {formatNumber(data.greeks.gamma, 6)}
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
                {formatNumber(data.greeks.vega, 4)}
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
                {formatNumber(data.greeks.theta, 4)}
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
                {formatNumber(data.greeks.rho, 4)}
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
