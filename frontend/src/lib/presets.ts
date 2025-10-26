import type { OptionKind } from "$lib/api/generated/types.gen"

export type Preset = {
  name: string
  description: string
  values: {
    spot: number
    strike: number
    timeToExpiry: { days: number }
    volatility: number
    riskFreeRate: number
    kind: OptionKind
  }
}

export const presets: Preset[] = [
  {
    name: "ATM BTC Call",
    description: "BTC @ $111k, strike $111k, 30d, 45% IV",
    values: {
      spot: 111000,
      strike: 111000,
      timeToExpiry: { days: 30 },
      volatility: 0.45,
      riskFreeRate: 0.039,
      kind: "Call",
    },
  },
  {
    name: "OTM BTC Call",
    description: "BTC @ $111k, strike $115k, 7d, 50% IV",
    values: {
      spot: 111000,
      strike: 115000,
      timeToExpiry: { days: 7 },
      volatility: 0.5,
      riskFreeRate: 0.039,
      kind: "Call",
    },
  },
  {
    name: "ITM BTC Put",
    description: "BTC @ $111k, strike $120k, 90d, 42% IV",
    values: {
      spot: 111000,
      strike: 120000,
      timeToExpiry: { days: 90 },
      volatility: 0.42,
      riskFreeRate: 0.039,
      kind: "Put",
    },
  },
]
