import System.Random
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	if length args /= 1
	then do
		return ()
	else do
		let numSamples = read $ head args :: Int
		gen <- getStdGen
		let rs = take numSamples $ randomRs (0,1) gen :: [Double]
		mapM_ print (map cardano rs)

cardano :: Double -> Double
cardano r = u - (1/u)
	where
		u = (-(q/2) + sqrt d)**(1/3 :: Double)
		d = 1 + (q**2 / 4)
		q = -8*r + 4
