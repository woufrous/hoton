module Hoton.DistributionsSpec (main, spec) where

import Test.Hspec
import Hoton.TestUtils

import Hoton.Distributions

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hoton.Distributions.RandomDistribution_" $ do
        it "should call sample when drawRandom is Called" $ do
            let (res1, g1') = drawRandom TestDistribution (DummyGen1 0)
            res1 `shouldSatisfy` (>=0)
            res1 `shouldSatisfy` (<=1)
            g1'  `shouldBe` (DummyGen1 0)
            let (res2, g2') = drawRandom TestDistribution (DummyGen1 maxBound)
            res2 `shouldSatisfy` (>=0)
            res2 `shouldSatisfy` (<=1)
            g2'  `shouldBe` (DummyGen1 maxBound)
    describe "Hoton.Distributions.Rayleigh" $ do
        it "evalues some examples correct" $ do
            sample Rayleigh 0   `shouldBeApprox` (-1)
            sample Rayleigh 0.4 `shouldBeApprox` (-0.2607566981843541)
            sample Rayleigh 0.5 `shouldBeApprox` (0)
            sample Rayleigh 0.7 `shouldBeApprox` (0.49331554017877377)
            sample Rayleigh 1   `shouldBeApprox` (1)
    describe "Hoton.Distributions.HenyeyGreenstein" $ do
        it "evalues some examples for g=0 correct" $ do
            sample (HenyeyGreenstein 0) 0 `shouldBeApprox` (-1)
            sample (HenyeyGreenstein 0) 1 `shouldBeApprox` 1
        it "evalues some examples for g=1 correct" $ do
            sample (HenyeyGreenstein 1) 0 `shouldBeApprox` 1
            sample (HenyeyGreenstein 1) 1 `shouldBeApprox` 1
        it "evalues some examples for g=-1 correct" $ do
            sample (HenyeyGreenstein (-1)) 0 `shouldBeApprox` (-1)
            sample (HenyeyGreenstein (-1)) 1 `shouldBeApprox` (-1)
        it "evalues some examples for g=0.85 correct" $ do
            sample (HenyeyGreenstein 0.85) 0    `shouldBeApprox` (-1)
            sample (HenyeyGreenstein 0.85) 0.01 `shouldBeApprox` (-0.6109819283588535)
            sample (HenyeyGreenstein 0.85) 0.1  `shouldBeApprox` (0.5708740234374996)
            sample (HenyeyGreenstein 0.85) 0.5  `shouldBeApprox` (0.9679375)
            sample (HenyeyGreenstein 0.85) 1    `shouldBeApprox` (1)
    describe "Hoton.Distributions.ThicknessDistribution" $ do
        it "evalues some examples correct" $ do
            sample ThicknessDistribution 0   `shouldBeApprox` 0
            sample ThicknessDistribution 0.5 `shouldBeApprox` 0.69314718055994529
            sample ThicknessDistribution 1   `shouldSatisfy`  isInfinite
    describe "Hoton.Distributions.AzimutalDistribution" $ do
        it "evalues some examples correct" $ do
            sample AzimutalDistribution 0    `shouldBeApprox` 0
            sample AzimutalDistribution 0.25 `shouldBeApprox` (0.5*pi)
            sample AzimutalDistribution 0.5  `shouldBeApprox` pi
            sample AzimutalDistribution 0.75 `shouldBeApprox` (1.5*pi)
            sample AzimutalDistribution 1    `shouldBeApprox` (2*pi)

data TestDistribution = TestDistribution deriving (Show)
instance RandomDistribution_ TestDistribution where
    sample TestDistribution r = r
