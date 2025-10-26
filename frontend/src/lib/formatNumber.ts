/**
 * Format a number with thousand separators and specified decimal places
 * @param value - The number to format
 * @param decimals - Number of decimal places (default: 4)
 * @returns Formatted number string with commas for thousands
 *
 * @example
 * formatNumber(4165.8931, 4) // "4,165.8931"
 * formatNumber(1234567.89, 2) // "1,234,567.89"
 * formatNumber(0.123456, 6) // "0.123456"
 */
export const formatNumber = (value: number, decimals: number = 4): string => {
  const fixed = value.toFixed(decimals)
  const [integerPart, decimalPart] = fixed.split(".")
  const formattedInteger = integerPart.replace(/\B(?=(\d{3})+(?!\d))/g, ",")
  return decimalPart ? `${formattedInteger}.${decimalPart}` : formattedInteger
}
