module Hoton.TestUtils
(
    approx,
    shouldBeApprox,
    shouldBeApproxM,
    shouldBeApproxV,
    DummyGen(..),
    DummyGen1(..),
) where

import Test.Hspec
import Test.HUnit.Lang

import System.Random

import Hoton.Matrix
import Hoton.Vector
import Debug.Trace

-- s: Should-Value
-- i: Is-Value
approx s i = abs (s-i) < 0.1^8
shouldBeApprox :: (Num a, Show a, Ord a, Fractional a) => a -> a -> Assertion
a `shouldBeApprox` b = a `shouldSatisfy` approx b

shouldBeApproxM :: Matrix3D -> Matrix3D -> Assertion
a `shouldBeApproxM` b = maxDiffs `shouldSatisfy` (< 0.1^8)
    where
        ok = maximum diffs < 0.1^8
        maxDiffs = if ok then maximum diffs else traceShow b $ traceShow a $ maximum diffs
        Matrix3D (Cartesian a11 a12 a13) (Cartesian a21 a22 a23) (Cartesian a31 a32 a33) = a
        Matrix3D (Cartesian b11 b12 b13) (Cartesian b21 b22 b23) (Cartesian b31 b32 b33) = b
        diffs = [(a11-b11),(a12-b12),(a13-b13),(a21-b21),(a22-b22),(a23-b23),(a31-b31),(a32-b32),(a33-b33)]

shouldBeApproxV :: Cartesian -> Cartesian -> Assertion
a `shouldBeApproxV` b = maxDiffs `shouldSatisfy` (< 0.1^8)
    where
        ok = maximum diffs < 0.1^8
        maxDiffs = if ok then maximum diffs else traceShow b $ traceShow a $ maximum diffs
        Cartesian a1 a2 a3 = a
        Cartesian b1 b2 b3 = b
        diffs = [(a1-b1),(a2-b2),(a3-b3)]

data DummyGen = DummyGen deriving (Show, Eq)
instance RandomGen DummyGen where
    next _  = (1, DummyGen)
    split _ = (DummyGen, DummyGen)

data DummyGen1 = DummyGen1 Int deriving (Show, Eq)
instance RandomGen DummyGen1 where
    next (DummyGen1 i)  = (i, (DummyGen1 i))
    split (DummyGen1 i) = ((DummyGen1 i), (DummyGen1 i))

