module Hoton.IO
(
    readAtmos,
) where

import System.IO

readAtmos :: FilePath -> IO [(Double,Double,Double)]
readAtmos fn = do
    contents <- readFile fn
    let ls = map (\[a,b,c] -> (read a::Double,read b::Double,read c::Double)) $ map words $ lines contents
    --print ls
    return ls
