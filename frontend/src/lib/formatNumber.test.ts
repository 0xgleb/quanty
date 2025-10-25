import { describe, it, expect } from "vitest"
import { formatNumber } from "./formatNumber"

describe("formatNumber", () => {
  describe("thousand separators", () => {
    it("formats thousands with commas", () => {
      expect(formatNumber(4165.8931, 4)).toBe("4,165.8931")
    })

    it("formats millions with commas", () => {
      expect(formatNumber(1234567.89, 2)).toBe("1,234,567.89")
    })

    it("formats billions with commas", () => {
      expect(formatNumber(1234567890.123, 3)).toBe("1,234,567,890.123")
    })

    it("does not add comma for numbers less than 1000", () => {
      expect(formatNumber(999.99, 2)).toBe("999.99")
    })

    it("handles numbers exactly at 1000", () => {
      expect(formatNumber(1000, 2)).toBe("1,000.00")
    })
  })

  describe("decimal places", () => {
    it("formats with 4 decimal places by default", () => {
      expect(formatNumber(123.456789)).toBe("123.4568")
    })

    it("formats with custom decimal places", () => {
      expect(formatNumber(123.456789, 2)).toBe("123.46")
      expect(formatNumber(123.456789, 6)).toBe("123.456789")
    })

    it("pads with zeros when needed", () => {
      expect(formatNumber(100, 4)).toBe("100.0000")
      expect(formatNumber(1.5, 3)).toBe("1.500")
    })
  })

  describe("small numbers", () => {
    it("formats numbers less than 1", () => {
      expect(formatNumber(0.123456, 6)).toBe("0.123456")
      expect(formatNumber(0.000001, 6)).toBe("0.000001")
    })

    it("handles zero", () => {
      expect(formatNumber(0, 4)).toBe("0.0000")
    })
  })

  describe("negative numbers", () => {
    it("formats negative numbers with thousand separators", () => {
      expect(formatNumber(-1234.56, 2)).toBe("-1,234.56")
    })

    it("formats negative millions", () => {
      expect(formatNumber(-1234567.89, 2)).toBe("-1,234,567.89")
    })
  })

  describe("rounding", () => {
    it("rounds up when decimal is >= 0.5", () => {
      expect(formatNumber(123.4567, 2)).toBe("123.46")
    })

    it("rounds down when decimal is < 0.5", () => {
      expect(formatNumber(123.4544, 2)).toBe("123.45")
    })
  })

  describe("edge cases", () => {
    it("handles very large numbers", () => {
      expect(formatNumber(999999999999.99, 2)).toBe("999,999,999,999.99")
    })

    it("handles very small decimals", () => {
      expect(formatNumber(0.00000001, 8)).toBe("0.00000001")
    })
  })
})
