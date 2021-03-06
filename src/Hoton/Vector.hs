module Hoton.Vector
(
    Vector3D (..),
    Cartesian (..),
    Spherical (..),
    toCartesian,
    toSpherical,
    anyPerpendicular,
) where

import Hoton.Types
import Control.Exception

class Vector3D v where
    scalar :: v -> v -> Number
    cross :: v -> v -> v
    normalize :: v -> v
    sdiv :: v -> Number -> v
    smul :: v -> Number -> v
    vadd :: v -> v -> v
    norm :: v -> Number

data Cartesian = Cartesian !Number !Number !Number deriving (Show, Eq)
instance Vector3D Cartesian where
    scalar (Cartesian x1 y1 z1) (Cartesian x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
    cross (Cartesian x1 y1 z1) (Cartesian x2 y2 z2) = Cartesian x3 y3 z3
        where
            x3 = (y1*z2)-(y2*z1)
            y3 = (z1*x2)-(z2*x1)
            z3 = (x1*y2)-(x2*y1)
    norm v = sqrt $ scalar v v
    _ `sdiv` 0 = throw DivideByZero
    (Cartesian x y z) `sdiv` n = Cartesian (x/n) (y/n) (z/n)
    (Cartesian x y z) `smul` n = Cartesian (x*n) (y*n) (z*n)
    (Cartesian x1 y1 z1) `vadd` (Cartesian x2 y2 z2) = Cartesian (x1+x2) (y1+y2) (z1+z2)
    normalize v = v `sdiv` norm v

data Spherical = Spherical Number Number Number deriving (Show)
instance Vector3D Spherical where
    scalar v1 v2 = scalar (toCartesian v1) (toCartesian v2)
    cross v1 v2 = toSpherical $ cross (toCartesian v1) (toCartesian v2)
    norm = norm . toCartesian
    v `sdiv` n = toSpherical $ toCartesian v `sdiv` n
    v `smul` n = toSpherical $ toCartesian v `smul` n
    v1 `vadd` v2 = toSpherical $ toCartesian v1 `vadd` toCartesian v2
    normalize = toSpherical . normalize . toCartesian

toCartesian :: Spherical -> Cartesian
toCartesian (Spherical r t p) = Cartesian (r * sin t * cos p) (r * sin t * sin p) (r * cos t)

toSpherical :: Cartesian -> Spherical
toSpherical (Cartesian x y z) = Spherical r t p
    where
        r = sqrt $ x**2 + y**2 + z**2
        t = acos z / r
        p
            | x>0           = atan (y/x)
            | x==0          = signum y * (pi/2)
            | x<0 && y>=0   = atan (y/x) + pi
            | otherwise     = atan (y/x) - pi

anyPerpendicular :: Cartesian -> Cartesian
anyPerpendicular (Cartesian x1 0  0) = Cartesian 0 1 0
anyPerpendicular (Cartesian _ x2 x3) = Cartesian 0 x3 (-x2)

