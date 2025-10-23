import GHC.Natural (Natural)
import Protolude
import Test.Hspec


data Asset = Asset
  { name :: Text
  , price :: Double -- TODO: use a more realistic type
  , holdings :: Natural
  }


value :: Asset -> Double
value Asset {..} = price * fromIntegral holdings


newtype Time = Time Natural
  deriving newtype (Num, Eq, Show)


riskFreeRate :: Double
riskFreeRate = 0.05


varRiskFreeRate :: Time -> Double
varRiskFreeRate _ = 0.05


yieldedBy :: Time -> Double
yieldedBy 0 = 1
yieldedBy 1 = 1 + riskFreeRate
yieldedBy t =
  (1 + varRiskFreeRate t) * yieldedBy (t - 1)


assets :: Time -> [Asset]
assets _ = []


investments :: Time -> Double
investments t =
  sum $
    (\asset -> fromIntegral (holdings asset) * price asset)
      <$> assets t


cash :: Time -> Double
cash _ = 0


wealth :: Time -> Double
wealth 0 =
  cash 0 * yieldedBy 0 + investments 0
wealth 1 =
  cash 1 * yieldedBy 1 + investments 1
wealth t =
  cash t * yieldedBy t + investments t


_wealthProcess :: Time -> Double
_wealthProcess t =
  cash t * yieldedBy t + investments t


gains :: Time -> Double
gains 1 = wealth 1 - wealth 0
gains _ = 0


_discounted :: Double -> Time -> Double
_discounted amount t = amount / yieldedBy t


-- discounted process t = process t / yieldedBy t

_priceChange :: Asset -> Asset -> Double
_priceChange assetAt0 assetAt1 = price assetAt1 - price assetAt0


wealthSpec :: Spec
wealthSpec = describe "wealth" do
  it "end of period wealth equals to the initial wealth + gains" do
    wealth 1 `shouldBe` wealth 0 + gains 1


gainsSpec :: Spec
gainsSpec = do
  describe "end of period wealth" do
    it "equals to the initial wealth + gains" do
      gains 1 `shouldBe` cash 1 * riskFreeRate + sum (value <$> assets 1)


-- it "holds the self-financing condition" do
--   wealth t
--     `shouldBe` cash (t + 1) * yieldedBy t
--       + sum ((\asset -> holdings asset (t + 1) * price asset t))

-- discountedPriceChange asset 1 =
--   discounted (price asset 1) - price asset 0

-- discountedGains 1 =
--   foldMap (\asset -> fromIntegral (holdings asset) * discountedPriceChange asset 1) $
--     assets 1

-- discountedWealth 1 = wealth 0 + fromIntegral (discountedGains 1)

main :: IO ()
main = hspec do
  wealthSpec
  gainsSpec
