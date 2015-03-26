import System.Random
import System.Random.Mersenne.Pure64
import System.Environment
import Text.Printf

import Hoton.Distributions
import Hoton.Vector
import Hoton.Types
import Hoton.Scene
import Hoton.Scenes.Forward1D
import Hoton.IO

main :: IO ()
main = do
    args <- getArgs
    exe <- getProgName
    if length args < 1
    then do
        putStrLn $ exe ++ " ACTION [params]"
        return ()
    else
        let (action:params) = args
        in dispatch action params

dispatch :: String -> ([String] -> IO ())
dispatch "t1"   = t1
dispatch "t2"   = t2
dispatch "t3"   = t3

t1 :: [String] -> IO ()
t1 params = do
    if length params /= 1
    then do
        exe <- getProgName
        putStrLn $ exe ++ " t1 nSamples"
        return ()
    else do
        let numSamples = read $ head params :: Int
        gen <- getStdGen
        let rs = take numSamples $ fst (drawRandoms (HenyeyGreenstein 0.85) gen numSamples)
        mapM_ print rs

t2 :: [String] -> IO ()
t2 params = do
    if length params /= 4
    then do
        exe <- getProgName
        putStrLn $ exe ++ " t2 g tau sza nPhotons"
        return ()
    else do
--        gen <- getStdGen
        gen <- newPureMT
        let [g, tau, sza] = map read $ take 3 params :: [Number]
            nphotons = read $ params !! 3
--        let top = BoundaryBox1D SourceTop
--        let bottom = BoundaryBox1D SourceBottom
--        let c = ContainerBox1D (Box (ContainerBox1D (Box top) (Box physics))) (Box bottom)
        let physics = physicsBox1D 1.0 tau $ RandomDistribution (HenyeyGreenstein g)
        print physics
        let
            pos0 = Cartesian 0.0 0.0 1.0
            dir0 = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
            (tau0, g') = drawRandom ThicknessDistribution gen
        printf "SZA=%.0f, DIR=%s\n" sza $ show dir0
        let (t,b) = summarize1D . take nphotons $ processManyEqualPhotons physics (Photon {pos=pos0,dir=dir0,tau_r=tau0}) g'
        let r' = t/(fromIntegral nphotons)
        let t' = b/(fromIntegral nphotons)
        printf "TOP=%.0f BOTTOM=%.0f T=%f R=%f\n" t b t' r'

t3 :: [String] -> IO ()
t3 params = do
    if length params /= 3
    then do
        exe <- getProgName
        putStrLn $ exe ++ " fn sza nphotons"
        return ()
    else do
        let fn:params' = params
        let sza        = read $ head params' :: Number
            nphotons   = read $ params' !! 1 :: Int
        atmos <- readAtmos fn
        let Just atmosphere = rayleighAtmos2Box atmos
        let Height h = getDim atmosphere
        gen <- newPureMT
        let
            pos0 = Cartesian 0.0 0.0 h
            dir0 = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
            (tau0, g') = drawRandom ThicknessDistribution gen
        let (t,b) = summarize1D . take nphotons $ processManyEqualPhotons atmosphere (Photon {pos=pos0,dir=dir0,tau_r=tau0}) g'
        let r' = t/(fromIntegral nphotons)
        let t' = b/(fromIntegral nphotons)
        printf "TOP=%.0f BOTTOM=%.0f R=%f T=%f\n" t b r' t'

