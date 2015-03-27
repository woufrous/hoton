--import System.Random
import System.Random.Mersenne
--import System.Random.Mersenne.Pure64
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
--        gen <- getStdGen
        gen <- newMTGen Nothing
        randomNumbers <- randoms gen
        let rs = take numSamples $ fst (drawRandoms (HenyeyGreenstein 0.85) randomNumbers numSamples)
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
--        gen <- newPureMT
        gen <- newMTGen Nothing
        randomNumbers <- randoms gen
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
            (ph0, g') = initializePhoton pos0 dir0 randomNumbers
        printf "SZA=%.0f, DIR=%s\n" sza $ show dir0
        let (t,b) = summarize1D . take nphotons $ processManyEqualPhotons physics ph0 g'
        let r' = t/(fromIntegral nphotons)
        let t' = b/(fromIntegral nphotons)
        printf "TOP=%.0f BOTTOM=%.0f T=%f R=%f\n" t b t' r'

t3 :: [String] -> IO ()
t3 params = do
    if length params /= 3
    then do
        exe <- getProgName
        putStrLn $ exe ++ " fn nphotons outFn"
        return ()
    else do
        let fn         = params !! 0
            nphotons   = read $ params !! 1 :: Int
            outFn      = params !! 2 :: String
        atmos <- readAtmos fn
        let Just atmosphere = rayleighAtmos2Box atmos
        let Height h = getDim atmosphere
--        gen <- newPureMT
        gen <- newMTGen Nothing
        let getTR gen sza = do
            randomNumbers <- randoms gen
            let
                pos0 = Cartesian 0.0 0.0 h
                dir0 = toCartesian $ Spherical 1.0 ((sza*pi/180)+pi) 0.0
                (ph0, g') = initializePhoton pos0 dir0 randomNumbers
                (t,b) = summarize1D . take nphotons $ processManyEqualPhotons atmosphere ph0 g'
            let r' = t/(fromIntegral nphotons)
            let t' = b/(fromIntegral nphotons)
            printf "TOP=%.0f BOTTOM=%.0f R=%f T=%f ABS=%.f\n" t b r' t' (1-r'-t')
            return (r', t')
        let angles = [0,5..85]
        res <- mapM (getTR gen) angles
        writeFile outFn $ unlines $ zipWith (\sza (r, t) -> printf "%.1f %.6f %.6f" sza r t) angles res
        print "DONE"

