{-# LANGUAGE ExistentialQuantification #-}
module Hoton.Distributions
( RandomDistribution (..),
  RandomDistribution_ (..),
  Rayleigh(..),
  HenyeyGreenstein(..),
  ThicknessDistribution(..),
  LambertDistribution(..),
  AzimutalDistribution(..),
) where

import System.Random

class Show rd => RandomDistribution_ rd where
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

data RandomDistribution = forall rd. RandomDistribution_ rd => RandomDistribution rd
instance RandomDistribution_ RandomDistribution where
    sample (RandomDistribution rd) = sample rd
    drawRandom (RandomDistribution rd) = drawRandom rd
    drawRandoms (RandomDistribution rd) = drawRandoms rd
instance Show RandomDistribution where
    show (RandomDistribution rd) = show rd

data Rayleigh = Rayleigh deriving (Show)
instance RandomDistribution_ Rayleigh where
    sample Rayleigh r = u - (1/u)
        where
            u = (-(q/2) + sqrt d)**(1/3 :: Double)
            d = 1 + (q**2 / 4)
            q = -8*r + 4


data HenyeyGreenstein = HenyeyGreenstein Double deriving (Show)
instance RandomDistribution_ HenyeyGreenstein where
    sample (HenyeyGreenstein   0.0) r = (2*r)-1
    sample (HenyeyGreenstein assym) r = u/(2*assym)
        where
            u = -v**2 + assym**2 + 1
            v = (1-assym**2)/w
            w = assym * (2*r - 1) + 1

data ThicknessDistribution = ThicknessDistribution deriving (Show)
instance RandomDistribution_ ThicknessDistribution where
    sample ThicknessDistribution r = - log (1 - r)

data LambertDistribution = LambertDistribution deriving (Show)
instance RandomDistribution_ LambertDistribution where
    sample LambertDistribution = sqrt

data AzimutalDistribution = AzimutalDistribution deriving (Show)
instance RandomDistribution_ AzimutalDistribution where
    sample AzimutalDistribution = (*(2*pi))

