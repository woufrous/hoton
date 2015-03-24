import Hoton.Types
import Hoton.Scene
import Hoton.Scenes.Forward1D

import Hoton.Distributions

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3
    then do
        print "args must be: g tau sza"
    else do
        let [g, tau, sza] = map read args :: [Number]
        print g
        print tau
        print sza
--        let top = BoundaryBox1D SourceTop
--        let bottom = BoundaryBox1D SourceBottom
        let physics = PhysicsBox1D 1.0 tau $ RandomDistribution (HenyeyGreenstein g)
--        let c = ContainerBox1D (Box (ContainerBox1D (Box top) (Box physics))) (Box bottom)
        print physics
        print $ processPhoton physics Photon



