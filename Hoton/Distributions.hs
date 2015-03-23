module Hoton.Distributions
( RandomDistribution (..),
  Rayleigh(..),
  HenyeyGreenstein(..)
) where

import System.Random

class RandomDistribution rd where
    drawRandom :: RandomGen g => rd -> g -> (Double, g)
    drawRandoms :: (RandomGen g, RandomDistribution rd, Integral n) => rd -> g -> n -> [Double]
    drawRandoms _ _ 0 = []
    drawRandoms rdf g n = x:drawRandoms rdf ng (n-1)
        where
            (x, ng) = drawRandom rdf g

data Rayleigh = Rayleigh
instance RandomDistribution Rayleigh where
    drawRandom Rayleigh g = ((u - (1/u)), g')
        where
            (r, g') = randomR (0,1) g
            u = (-(q/2) + sqrt d)**(1/3 :: Double)
            d = 1 + (q**2 / 4)
            q = -8*r + 4

data HenyeyGreenstein = HenyeyGreenstein Double
instance RandomDistribution HenyeyGreenstein where
    drawRandom (HenyeyGreenstein assym) g = (u/(2*assym), g')
        where
            (r, g') = randomR (0,1) g
            u = -v**2 + assym**2 + 1
            v = (1-assym**2)/w
            w = assym * (2*r - 1) + 1

