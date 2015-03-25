module Hoton.VectorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Hoton.Types
import Hoton.Vector

-- p: Number of decimal places
-- s: Should-Value
-- i: Is-Value
approx p s i = abs (s-i) < 0.1^p

spec :: Spec
spec = do
    describe "Hoton.Vector.scalar" $ do
        it "returns 0 for perpendicular vectors" $ do
            Cartesian 1 0 0 `scalar` Cartesian 0 1 0 `shouldBe` (0 :: Number)
            Cartesian 1 1 1 `scalar` Cartesian 0.5 (-1) 0.5 `shouldBe` (0 :: Number)
        it "returns 1 for parallel unit vectors" $ do
            approx 8 1 (Cartesian (sqrt 0.5) 0 (sqrt 0.5)
                        `scalar`
                        Cartesian (sqrt 0.5) 0 (sqrt 0.5)) `shouldBe` True
    describe "Hoton.Vector.norm" $ do
        it "returns 1 for unit vectors" $ do
            approx 8 1 (norm (Cartesian 1 0 0)) `shouldBe` True

