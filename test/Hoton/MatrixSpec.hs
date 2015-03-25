module Hoton.MatrixSpec (main, spec) where

import Test.Hspec

import Hoton.Matrix
import Hoton.Vector

import Hoton.TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hoton.Matrix.mvmul" $ do
        it "left-multiplies vector with identity matrix" $
            meye `mvmul` Cartesian (-1.0) 2 3 `shouldBe` Cartesian (-1.0) 2 3
        it "left-multiplies 0-vector with identity matrix" $
            meye `mvmul` Cartesian 0 0 0 `shouldBe` Cartesian 0 0 0
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
    describe "Hoton.Matrix.mrotax" $ do
        it "rotates x unit vector around y to get -z unit vector" $
            mrotax (pi/2) ey `mvmul` ex `shouldBeApproxV` (ez `smul` (-1))
            where
                ex = Cartesian 1 0 0
                ey = Cartesian 0 1 0
                ez = Cartesian 0 0 1

