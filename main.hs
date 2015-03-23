import System.Random

main :: IO ()
main = do
	gen <- getStdGen
	let rs = take 10 $ randomRs (0,1) gen :: [Double]
	mapM_ print (map cardano rs)

cardano :: Double -> Double
cardano r = u - (1/u)
	where
		u = (-(q/2) + sqrt d)**(1/3 :: Double)
		d = 1 + (q**2 / 4)
		q = -8*r + 4
