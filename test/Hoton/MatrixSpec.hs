module Hoton.MatrixSpec (main, spec) where

import Test.Hspec

import Hoton.Matrix
import Hoton.Vector

import Hoton.TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let ex = Cartesian 1 0 0
    let ey = Cartesian 0 1 0
    let ez = Cartesian 0 0 1
    describe "Hoton.Matrix.mvmul" $ do
        it "left-multiplies vector with identity matrix" $ do
            meye `mvmul` Cartesian (-1.0) 2 3 `shouldBe` Cartesian (-1.0) 2 3
        it "left-multiplies 0-vector with identity matrix" $ do
            meye `mvmul` Cartesian 0 0 0 `shouldBe` Cartesian 0 0 0
        it "changes rows of a vector" $ do
            let v123 = Cartesian 1 2 3
                c m = (mgen m `mvmul` v123)
            c ((1,0,0),(0,0,1),(0,1,0)) `shouldBe` Cartesian 1 3 2
            c ((0,0,1),(1,0,0),(0,1,0)) `shouldBe` Cartesian 3 1 2
    describe "Hoton.Matrix.mrot{x,y,z}" $ do
        it "creates a matrix for rotation around x" $
            mrotx pi `shouldBeApproxM` mgen ((1,0,0), (0,(-1),0), (0,0,(-1)))
        it "creates a matrix for rotation around y" $
            mroty pi `shouldBeApproxM` mgen (((-1),0,0), (0,(1),0), (0,0,(-1)))
        it "creates a matrix for rotation around z" $
            mrotz pi `shouldBeApproxM` mgen (((-1),0,0), (0,(-1),0), (0,0,1))
        it "rotates by 180deg clockwise and counter clockwise around x which should be equal" $
            mrotx pi `shouldBeApproxM` mrotx (-pi)
        it "rotates by 180deg clockwise and counter clockwise around y which should be equal" $
            mroty pi `shouldBeApproxM` mroty (-pi)
        it "rotates by 180deg clockwise and counter clockwise around z which should be equal" $
            mrotz pi `shouldBeApproxM` mrotz (-pi)
        it "rotates by 45deg clockwise and 315deg counter clockwise around x which should be equal" $
            mrotx (pi/4) `shouldBeApproxM` mrotx ((-pi)*7/4)
        it "rotates by 45deg clockwise and 315deg counter clockwise around y which should be equal" $
            mroty (pi/4) `shouldBeApproxM` mroty ((-pi)*7/4)
        it "rotates by 45deg clockwise and 315deg counter clockwise around z which should be equal" $
            mrotz (pi/4) `shouldBeApproxM` mrotz ((-pi)*7/4)
    describe "Hoton.Matrix.mrotax" $ do
        it "rotates x unit vector around y to get -z unit vector" $
            mrotax (pi/2) ey `mvmul` ex `shouldBeApproxV` (ez `smul` (-1))
    describe "Hoton.Matrix.mrotaxmu" $ do
        it "rotates x unit vector around y to get -z unit vector" $
            mrotaxmu 0 ey `mvmul` ex `shouldBeApproxV` (ez `smul` (-1))
        it "rotates x unit vector around y to get diagonal unit vector" $
            mrotaxmu 0.5 ey `mvmul` ex `shouldBeApproxV` (normalize (Cartesian 1 0 (-1)))
