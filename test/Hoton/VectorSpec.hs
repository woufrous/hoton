module Hoton.VectorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Hoton.TestUtils

import Hoton.Types
import Hoton.Vector

t2cart (x1,x2,x3) = Cartesian x1 x2 x3

spec :: Spec
spec = do
    describe "Hoton.Vector.scalar" $ do
        it "returns 0 for perpendicular vectors" $ do
            Cartesian 1 0 0 `scalar` Cartesian 0 1 0 `shouldBe` (0 :: Number)
            Cartesian 1 1 1 `scalar` Cartesian 0.5 (-1) 0.5 `shouldBe` (0 :: Number)
        it "returns 1 for parallel unit vectors" $ do
            (Cartesian (sqrt 0.5) 0 (sqrt 0.5) `scalar` Cartesian (sqrt 0.5) 0 (sqrt 0.5)) `shouldBeApprox` 1
    describe "Hoton.Vector.smul" $ do
        it "returns double of the vector" $ do
            Cartesian 1 3 (-4) `smul` 2 `shouldBe` Cartesian 2 6 (-8)
    describe "Hoton.Vector.sdiv" $ do
        it "returns half of the vector" $ do
            Cartesian 1 3 (-4) `sdiv` 2 `shouldBe` Cartesian 0.5 1.5 (-2)
        it "throws DivideByZero if divides by zero" $ do
            evaluate ((Cartesian 1 3 (-4)) `sdiv` 0) `shouldThrow` (==DivideByZero)
    describe "Hoton.Vector.vadd" $ do
        it "returns the sum" $ do
            Cartesian 1 (-2) 7 `vadd` Cartesian 3 5 (-2) `shouldBe` Cartesian 4 3 5
    describe "Hoton.Vector.norm" $ do
        it "returns 1 for unit vectors" $ do
            norm (Cartesian 1 0 0) `shouldBeApprox` 1
    describe "Hoton.Vector.normalize" $ do
        it "returns unit vector for any non-zero vector" $ do
            mapM_ ((`shouldBeApprox` 1) . norm . normalize . t2cart)
                [(1,2,3), (0,1,4), (-1,5,6), (9,0,4), (-3,2,-7)]
        it "throws DivideByZero for zero vector" $ do
            evaluate (normalize (Cartesian 0 0 0)) `shouldThrow` (==DivideByZero)
    describe "Hoton.Vector.toCartesian" $ do
        it "resturns (0 0 1) for theta=0, phi=0" $ do
            toCartesian (Spherical 1 0 0) `shouldBeApproxV` (Cartesian 0 0 1)
        it "resturns normalized (1 0 1) for theta=pi/4, phi=0" $ do
            toCartesian (Spherical 1 (pi/4) 0) `shouldBeApproxV` normalize (Cartesian 1 0 1)
        it "resturns normalized (1 1 (sqrt2)) for theta=pi/4, phi=pi/4" $ do
            toCartesian (Spherical 1 (pi/4) (pi/4)) `shouldBeApproxV` normalize (Cartesian 1 1 (sqrt 2))
