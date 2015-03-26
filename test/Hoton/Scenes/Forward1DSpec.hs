module Hoton.Scenes.Forward1DSpec (spec) where

import Test.Hspec

import Hoton.TestUtils
import Hoton.Vector
import Hoton.Types
import Hoton.Distributions

import Hoton.Scene
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
    describe "Hoton.Scenes.Forward1D.PhysicsBox1D.processPhoton" $ do
        it "returns a bottom photon when travelling from top with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let start    = Cartesian 0 0 1
            let dir      = Cartesian 0 0 (-1)
            let out      = Cartesian 0 0 0
            let (res, _) = processPhoton physics (Photon{pos=start, dir=dir, tau_r=2}) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceBottom
            ph `shouldBe` Photon{pos=out, dir=dir, tau_r=1}
        it "returns a top photon when travelling from bottom with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let start    = Cartesian 0 0 0
            let dir      = normalize $ Cartesian 0 1 1
            let out      = Cartesian 0 1 1
            let (res, _) = processPhoton physics (Photon{pos=start, dir=dir, tau_r=2}) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceTop
            ph `shouldBe` Photon{pos=out, dir=dir, tau_r=(2-(sqrt 2))}
    describe "Hoton.Scenes.Forward1D.ContainerBox1D.processPhoton" $ do
        it "returns a bottom photon when travelling from top with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let cont     = containerBox1D physics physics
            let start    = Cartesian 0 0 2
            let dir      = Cartesian 0 0 (-1)
            let out      = Cartesian 0 0 0
            let (res, _) = processPhoton cont (Photon{pos=start, dir=dir, tau_r=3}) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceBottom
            ph `shouldBe` Photon{pos=out, dir=dir, tau_r=1}
        it "returns a top photon when travelling from bottom with big tau" $ do
            let physics1 = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let physics2 = physicsBox1D 2 2 $ RandomDistribution (HenyeyGreenstein 0.85)
            let cont     = containerBox1D physics1 physics2
            let start    = Cartesian 0 0 0
            let dir      = normalize $ Cartesian 0 1 1
            let out      = Cartesian 0 3 3
            let (res, _) = processPhoton cont (Photon{pos=start, dir=dir, tau_r=10}) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceTop
            ph `shouldBe` Photon{pos=out, dir=dir, tau_r=(10-(5*(sqrt 2)))}


