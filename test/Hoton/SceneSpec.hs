module Hoton.SceneSpec (main, spec) where

import Test.Hspec

import Hoton.Vector
import Hoton.Scene
import Hoton.TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hoton.Scene.posScat" $ do
        it "returns the scattering position of a given photon" $ do
            let ph = Photon{pos=Cartesian 0 0 1,dir=Cartesian 0 1 0,tau_r=5}
            posScat ph 1 `shouldBeApproxV` Cartesian 0 5 1
            posScat ph 2 `shouldBeApproxV` Cartesian 0 2.5 1