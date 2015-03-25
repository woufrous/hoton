module Hoton.Scenes.Forward1DSpec (spec) where

import Test.Hspec

import Hoton.TestUtils
import Hoton.Vector
import Hoton.Types

import Hoton.Scenes.Forward1D

spec :: Spec
spec = do
    describe "Hoton.Scenes.Forward1D.newDirection" $ do
        it "returns a vector rotated by mu" $ do
            let v1 = Cartesian 1 0 0
            let mu = 1.0
            let phi = 1.0
            normalize v1 `scalar` newDirection v1 mu phi `shouldBeApprox` mu
            let v2 = Cartesian 1 1 0
            normalize v2 `scalar` newDirection v2 mu phi `shouldBeApprox` mu

