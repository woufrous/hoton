module Hoton.Distributions
( RandomDistribution (..),
  Rayleigh(..),
  HenyeyGreenstein(..),
  ThicknessDistribution(..),
  LambertDistribution(..)
) where

import System.Random

class RandomDistribution rd where
    sample :: rd -> Double -> Double
    drawRandom  :: (RandomGen g) => rd -> g -> (Double, g)
    drawRandom rdf g = (sample rdf r, g')
        where
            (r, g') = randomR (0,1) g
    drawRandoms :: (RandomGen g, Integral n) => rd -> g -> n -> ([Double], g)
    drawRandoms _ g 0 = ([], g)
    drawRandoms rdf g n = (x:xs, ng')
        where
            (x, ng) = drawRandom rdf g
            (xs, ng') = drawRandoms rdf ng (n-1)

data Rayleigh = Rayleigh
instance RandomDistribution Rayleigh where
    sample Rayleigh r = u - (1/u)
        where
            u = (-(q/2) + sqrt d)**(1/3 :: Double)
            d = 1 + (q**2 / 4)
            q = -8*r + 4


data HenyeyGreenstein = HenyeyGreenstein Double
instance RandomDistribution HenyeyGreenstein where
    sample (HenyeyGreenstein assym) r = u/(2*assym)
        where
            u = -v**2 + assym**2 + 1
            v = (1-assym**2)/w
            w = assym * (2*r - 1) + 1

data ThicknessDistribution = ThicknessDistribution
instance RandomDistribution ThicknessDistribution where
    sample ThicknessDistribution r = - log (1 - r)

data LambertDistribution = LambertDistribution
instance RandomDistribution LambertDistribution where
    sample LambertDistribution = sqrt

