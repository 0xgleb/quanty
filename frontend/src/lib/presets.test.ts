import { describe, it, expect } from "vitest"
import { presets } from "./presets"

describe("Calculator Presets", () => {
  describe("preset structure", () => {
    it("has 3 presets", () => {
      expect(presets).toHaveLength(3)
    })

    it("all presets have required fields", () => {
      presets.forEach(preset => {
        expect(preset).toHaveProperty("name")
        expect(preset).toHaveProperty("description")
        expect(preset).toHaveProperty("values")
        expect(preset.values).toHaveProperty("spot")
        expect(preset.values).toHaveProperty("strike")
        expect(preset.values).toHaveProperty("timeToExpiry")
        expect(preset.values).toHaveProperty("volatility")
        expect(preset.values).toHaveProperty("riskFreeRate")
        expect(preset.values).toHaveProperty("kind")
      })
    })
  })

  describe("ATM BTC Call preset", () => {
    const atmCall = presets.find(p => p.name === "ATM BTC Call")!

    it("exists", () => {
      expect(atmCall).toBeDefined()
    })

    it("has matching spot and strike prices", () => {
      expect(atmCall.values.spot).toBe(atmCall.values.strike)
    })

    it("is a Call option", () => {
      expect(atmCall.values.kind).toBe("Call")
    })

    it("has positive values", () => {
      expect(atmCall.values.spot).toBeGreaterThan(0)
      expect(atmCall.values.strike).toBeGreaterThan(0)
      expect(atmCall.values.timeToExpiry.days).toBeGreaterThan(0)
      expect(atmCall.values.volatility).toBeGreaterThan(0)
      expect(atmCall.values.riskFreeRate).toBeGreaterThanOrEqual(0)
    })

    it("has realistic volatility (0-2)", () => {
      expect(atmCall.values.volatility).toBeGreaterThan(0)
      expect(atmCall.values.volatility).toBeLessThan(2)
    })
  })

  describe("OTM BTC Call preset", () => {
    const otmCall = presets.find(p => p.name === "OTM BTC Call")!

    it("exists", () => {
      expect(otmCall).toBeDefined()
    })

    it("has strike > spot (out of the money)", () => {
      expect(otmCall.values.strike).toBeGreaterThan(otmCall.values.spot)
    })

    it("is a Call option", () => {
      expect(otmCall.values.kind).toBe("Call")
    })

    it("has positive values", () => {
      expect(otmCall.values.spot).toBeGreaterThan(0)
      expect(otmCall.values.strike).toBeGreaterThan(0)
      expect(otmCall.values.timeToExpiry.days).toBeGreaterThan(0)
      expect(otmCall.values.volatility).toBeGreaterThan(0)
      expect(otmCall.values.riskFreeRate).toBeGreaterThanOrEqual(0)
    })
  })

  describe("ITM BTC Put preset", () => {
    const itmPut = presets.find(p => p.name === "ITM BTC Put")!

    it("exists", () => {
      expect(itmPut).toBeDefined()
    })

    it("has strike > spot (in the money for put)", () => {
      expect(itmPut.values.strike).toBeGreaterThan(itmPut.values.spot)
    })

    it("is a Put option", () => {
      expect(itmPut.values.kind).toBe("Put")
    })

    it("has positive values", () => {
      expect(itmPut.values.spot).toBeGreaterThan(0)
      expect(itmPut.values.strike).toBeGreaterThan(0)
      expect(itmPut.values.timeToExpiry.days).toBeGreaterThan(0)
      expect(itmPut.values.volatility).toBeGreaterThan(0)
      expect(itmPut.values.riskFreeRate).toBeGreaterThanOrEqual(0)
    })
  })

  describe("preset descriptions", () => {
    it("ATM preset description shows key parameters", () => {
      const atmCall = presets.find(p => p.name === "ATM BTC Call")!
      expect(atmCall.description).toContain("111k")
      expect(atmCall.description).toContain("30d")
      expect(atmCall.description).toContain("45%")
    })

    it("OTM preset description shows key parameters", () => {
      const otmCall = presets.find(p => p.name === "OTM BTC Call")!
      expect(otmCall.description).toContain("111k")
      expect(otmCall.description).toContain("115k")
      expect(otmCall.description).toContain("7d")
      expect(otmCall.description).toContain("50%")
    })

    it("ITM preset description shows key parameters", () => {
      const itmPut = presets.find(p => p.name === "ITM BTC Put")!
      expect(itmPut.description).toContain("111k")
      expect(itmPut.description).toContain("120k")
      expect(itmPut.description).toContain("90d")
      expect(itmPut.description).toContain("42%")
    })
  })
})
