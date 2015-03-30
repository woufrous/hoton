module Hoton.IO
(
    readAtmos,
) where

import System.IO
import Data.List

readAtmos :: FilePath -> IO [(Double,Double,Double)]
readAtmos fn = do
    contents <- readFile fn
    let ls = map ((\[a,b,c] -> (read a::Double,read b::Double,read c::Double)) . words) $ lines contents
    --print ls
    return $ sortBy (\(a,_,_) (b,_,_) -> a `compare` b) ls

