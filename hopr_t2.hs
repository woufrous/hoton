import Hoton.Types
import Hoton.Scene
import Hoton.Scenes.Forward1D

import Hoton.Distributions
import Hoton.Vector

import System.Environment
import System.Random

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3
    then do
        print "args must be: g tau sza"
    else do
        gen <- getStdGen
        let [g, tau, sza] = map read args :: [Number]
        print g
        print tau
        print sza
--        let top = BoundaryBox1D SourceTop
--        let bottom = BoundaryBox1D SourceBottom
        let physics = PhysicsBox1D 1.0 tau $ RandomDistribution (HenyeyGreenstein g)
--        let c = ContainerBox1D (Box (ContainerBox1D (Box top) (Box physics))) (Box bottom)
        print physics
        let
            pos0 = Cartesian 0.0 0.0 1.0
            dir0 = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
            (tau0, g') = drawRandom ThicknessDistribution gen
        print $ processPhoton physics (Photon {pos=pos0,dir=dir0,tau_r=tau0}) g'

