module Hoton.Distributions
( RandomDistribution (..),
  Rayleigh(..),
  HenyeyGreenstein(..),
  ThicknessDistribution(..),
  LambertDistribution(..)
) where

import System.Random

class RandomDistribution rd where
    drawRandom  :: RandomGen g => rd -> g -> (Double, g)
    drawRandoms :: (RandomGen g, RandomDistribution rd, Integral n) => rd -> g -> n -> ([Double], g)
    drawRandoms _ g 0 = ([], g)
    drawRandoms rdf g n = (x:xs, ng')
        where
            (x, ng) = drawRandom rdf g
            (xs, ng') = drawRandoms rdf ng (n-1)

trans :: RandomGen g => (Double -> Double) -> g -> (Double, g)
trans f g = (f r, g')
    where
        (r, g') = randomR (0,1) g

data Rayleigh = Rayleigh
instance RandomDistribution Rayleigh where
    drawRandom Rayleigh = trans (\r -> let
            u = (-(q/2) + sqrt d)**(1/3 :: Double)
            d = 1 + (q**2 / 4)
            q = -8*r + 4
            in
            u - (1/u))


data HenyeyGreenstein = HenyeyGreenstein Double
instance RandomDistribution HenyeyGreenstein where
    drawRandom (HenyeyGreenstein assym) g = (u/(2*assym), g')
        where
            (r, g') = randomR (0,1) g
            u = -v**2 + assym**2 + 1
            v = (1-assym**2)/w
            w = assym * (2*r - 1) + 1

data ThicknessDistribution = ThicknessDistribution
instance RandomDistribution ThicknessDistribution where
    drawRandom ThicknessDistribution = trans (\r -> (- log (1 - r)))

data LambertDistribution = LambertDistribution
instance RandomDistribution LambertDistribution where
    drawRandom LambertDistribution = trans sqrt

