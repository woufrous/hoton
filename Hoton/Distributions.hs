module Hoton.Distributions
( RandomDistribution,
  Rayleigh(..),
  HenyeyGreenstein(..),
  drawRandom
) where

import System.Random

class RandomDistribution rd where
    drawRandom :: RandomGen g => rd -> g -> (Double, g)

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
    drawRandom (HenyeyGreenstein assym) g = (assym, g)

