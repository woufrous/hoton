module Hoton.SceneSpec (main, spec) where

import Test.Hspec

import Hoton.Vector
import Hoton.Scene
import Hoton.Distributions

import Hoton.TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hoton.Scene.posScat" $ do
        it "returns the scattering position of a given photon" $ do
            let ph = initializePhotonWithTau 5 (Cartesian 0 0 1) (Cartesian 0 1 0)
            posScat ph (Scatterer 0 1 $ RandomDistribution Rayleigh) `shouldBeApproxV` Cartesian 0 5 1
            posScat ph (Scatterer 0 2 $ RandomDistribution Rayleigh) `shouldBeApproxV` Cartesian 0 2.5 1
    describe "Hoton.Scene.newDirection" $ do
        it "returns a vector rotated by mu" $ do
            let v1 = Cartesian 1 0 0
            let mu = 1.0
            let phi = 1.0
            normalize v1 `scalar` newDirection v1 mu phi `shouldBeApprox` mu
            let v2 = Cartesian 1 1 0
            normalize v2 `scalar` newDirection v2 mu phi `shouldBeApprox` mu

