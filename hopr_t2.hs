import Hoton.Types
import Hoton.Scene
import Hoton.Scenes.Forward1D

import Hoton.Distributions
import Hoton.Vector

import System.Environment
import System.Random

import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    if length args /= 4
    then do
        print "args must be: g tau sza nphotons"
    else do
        gen <- getStdGen
        let [g, tau, sza] = map read $ take 3 args :: [Number]
            nphotons = read $ args !! 3
--        let top = BoundaryBox1D SourceTop
--        let bottom = BoundaryBox1D SourceBottom
        let physics = PhysicsBox1D 1.0 tau $ RandomDistribution (HenyeyGreenstein g)
--        let c = ContainerBox1D (Box (ContainerBox1D (Box top) (Box physics))) (Box bottom)
        print physics
        let
            pos0 = Cartesian 0.0 0.0 1.0
            dir0 = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
            (tau0, g') = drawRandom ThicknessDistribution gen
        let (t,b) = summarize1D . take nphotons $ processManyEqualPhotons physics (Photon {pos=pos0,dir=dir0,tau_r=tau0}) g'
        let r' = t/(fromIntegral nphotons)
        let t' = b/(fromIntegral nphotons)
        printf "TOP=%.0f BOTTOM=%.0f R=%f T=%f\n" t b r' t'

