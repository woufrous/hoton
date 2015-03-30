module Hoton.Scenes.Forward1DSpec (spec) where

import Test.Hspec

import Hoton.TestUtils
import Hoton.Vector
import Hoton.Types
import Hoton.Distributions

import Hoton.Scene
import Hoton.Scenes.Forward1D

import Data.Vector.Unboxed (singleton, fromList)

-- from: http://stackoverflow.com/questions/16248600/parallel-computations-with-fast-randomness-and-purity
import Control.Monad.ST.Lazy
import Control.Monad
import System.Random.MWC

rList :: Variate a => Seed -> [a]
rList s = runST $ do
  g <- strictToLazyST $ restore s
  advance g

advance :: Variate a => Gen s -> ST s [a]
advance g = do
  x <- strictToLazyST $ uniform g
  xs <- x `seq` advance g
  return (x:xs)
-- end from

spec :: Spec
spec = do
    describe "Hoton.Scenes.Forward1D.PhysicsBox1D.processPhoton" $ do
        it "returns a bottom photon when travelling from top with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let start    = Cartesian 0 0 1
            let dir      = Cartesian 0 0 (-1)
            let (res, _) = processPhoton physics (initializePhotonWithTau 2 start dir) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceBottom
            tauR ph `shouldBe` 1
            pos ph  `shouldBe` (Cartesian 0 0 0)
        it "returns a top photon when travelling from bottom with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let start    = Cartesian 0 0 0
            let dir      = normalize $ Cartesian 0 1 1
            let (res, _) = processPhoton physics (initializePhotonWithTau 2 start dir) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceTop
            tauR ph `shouldBe` (2-(sqrt 2))
            pos ph  `shouldBe` (Cartesian 0 1 1)
    describe "Hoton.Scenes.Forward1D.ContainerBox1D.processPhoton" $ do
        it "returns a bottom photon when travelling from top with big tau" $ do
            let physics  = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let cont     = containerBox1D physics physics
            let start    = Cartesian 0 0 2
            let dir      = Cartesian 0 0 (-1)
            let (res, _) = processPhoton cont (initializePhotonWithTau 3 start dir) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceBottom
            tauR ph `shouldBe` 1
            pos ph  `shouldBe` (Cartesian 0 0 0)
        it "returns a top photon when travelling from bottom with big tau" $ do
            let physics1 = physicsBox1D 1 1 $ RandomDistribution (HenyeyGreenstein 0.85)
            let physics2 = physicsBox1D 2 2 $ RandomDistribution (HenyeyGreenstein 0.85)
            let cont     = containerBox1D physics1 physics2
            let start    = Cartesian 0 0 0
            let dir      = normalize $ Cartesian 0 1 1
            let (res, _) = processPhoton cont (initializePhotonWithTau 10 start dir) [0..]
            let [IRPhoton f ph] = res
            f  `shouldBe` FaceTop
            tauR ph `shouldBe` (10-(5*(sqrt 2)))
            pos ph  `shouldBe` (Cartesian 0 3 3)
    describe "single layer statistical examples" $ do
        let pos0 = Cartesian 0.0 0.0 1.0
        let nphotons = 100
        let dir0 sza = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
        let randomNumbers1 = rList $ toSeed (fromList [2133254,1346373547263,2572345,2357568245,351345,6458643,2363])
        let randomNumbers2 = rList $ toSeed (fromList [113253345,23626,34267234,334573,23465,465835672])
        let iPh sza = fst $ initializePhoton pos0 (dir0 sza) randomNumbers1
        let mkPhysics g tau = physicsBox1D 1.0 tau $ RandomDistribution (HenyeyGreenstein g)
        let rt (t,b) = (t/(fromIntegral nphotons), b/(fromIntegral nphotons))
        let doCalc g tau sza = rt $ summarize1D . take nphotons $ processManyEqualPhotons (mkPhysics g tau) (iPh sza) randomNumbers2
        it "should increase reflectivity with optical thickness" $ do
            let (r0,t0) = doCalc 0.85 1 0
            r0 `shouldSatisfy` (>0)
            r0 `shouldSatisfy` (<0.05)
            let (r1,t1) = doCalc 0.85 10 0
            r1 `shouldSatisfy` (>0.36)
            r1 `shouldSatisfy` (<0.42)
            let (r2,t2) = doCalc 0.85 100 0
            r2 `shouldSatisfy` (>0.85)
            r2 `shouldSatisfy` (<0.9)

