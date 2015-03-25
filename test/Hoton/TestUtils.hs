module Hoton.TestUtils
(
    approx,
    shouldBeApprox,
    shouldBeApproxM,
    shouldBeApproxV,
) where

import Test.Hspec
import Test.HUnit.Lang

import Hoton.Matrix
import Hoton.Vector

-- s: Should-Value
-- i: Is-Value
approx s i = abs (s-i) < 0.1^8
shouldBeApprox :: (Num a, Show a, Ord a, Fractional a) => a -> a -> Assertion
a `shouldBeApprox` b = a `shouldSatisfy` approx b

shouldBeApproxM :: Matrix3D -> Matrix3D -> Assertion
a `shouldBeApproxM` b = maximum diffs `shouldSatisfy` (< 0.1^8)
    where
        Matrix3D (Cartesian a11 a12 a13) (Cartesian a21 a22 a23) (Cartesian a31 a32 a33) = a
        Matrix3D (Cartesian b11 b12 b13) (Cartesian b21 b22 b23) (Cartesian b31 b32 b33) = b
        diffs = [(a11-b11),(a12-b12),(a13-b13),(a21-b21),(a22-b22),(a23-b23),(a31-b31),(a32-b32),(a33-b33)]

shouldBeApproxV :: Cartesian -> Cartesian -> Assertion
a `shouldBeApproxV` b = maximum diffs `shouldSatisfy` (< 0.1^8)
    where
        Cartesian a1 a2 a3 = a
        Cartesian b1 b2 b3 = b
        diffs = [(a1-b1),(a2-b2),(a3-b3)]
