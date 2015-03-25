import System.Random
import System.Environment

import Hoton.Distributions

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then do
        return ()
    else do
        let numSamples = read $ head args :: Int
        gen <- getStdGen
        let rs = take numSamples $ fst (drawRandoms (HenyeyGreenstein 0.85) gen numSamples)
        mapM_ print rs

