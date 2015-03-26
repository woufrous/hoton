{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hoton.Scene
(
    Photon(..),
    posScat,
    Source(..),
    Source_(..),
    SourceOrSink(..),
    InteractionResult(..),
    Box(..),
    Box_(..),
    Face(..),
    Dimensions(..),
    BoxLevel(..),
    --PhysicsBox(..),
    --BoundaryBox(..),
) where

import Hoton.Types
import Hoton.Vector
import Hoton.Distributions

import System.Random

data Photon = Photon {
    tau_r :: !Number,
    pos :: !Cartesian,
    dir :: !Cartesian
} deriving (Show, Eq)

posScat :: Photon -> Number -> Cartesian
posScat ph beta = ((dir ph) `smul` ((tau_r ph)/beta)) `vadd` (pos ph)

class Show s => Source_ s

data Source = forall s. Source_ s => Source s
instance Source_ Source
instance Show Source where
    show (Source s) = show s

data SourceOrSink = SoSSource Source | Sink Number deriving (Show)

data family Face boxType
data family Dimensions boxType
data family BoxLevel boxType

data InteractionResult faceType = IRPhoton faceType Photon | IRSoS SourceOrSink deriving (Show)

data Box bFamily = forall b. Box_ bFamily b => Box bFamily b

instance Show (Box bFamily) where
    show (Box _ b) = show b

class Show b => Box_ bFamily b where
    processPhoton :: b -> Photon -> [Number] -> ([InteractionResult (Face bFamily)], [Number])
    processManyEqualPhotons :: b -> Photon -> [Number] -> [InteractionResult (Face bFamily)]
    processManyEqualPhotons b ph r = res' ++ processManyEqualPhotons b ph_new  r''
        where
            (res', r')       = processPhoton b ph r
            (tau_r_new, r'') = drawRandom ThicknessDistribution r'
            ph_new           = Photon tau_r_new (pos ph) (dir ph)
    getDim :: b -> (Dimensions bFamily)
    addBox :: b -> (Box bFamily) -> (Box bFamily)
    boxLevel :: b -> (BoxLevel bFamily)

instance Box_ bFamily (Box bFamily) where
    processPhoton (Box _ b) ph r = processPhoton b ph r
    getDim        (Box _ b)      = getDim b
    addBox        (Box _ b) b2   = addBox b b2
    boxLevel      (Box _ b)      = boxLevel b

-- data PhysicsBox = PhysicsBox {
--     height :: Number,
--     beta :: Number
--     } deriving (Show)
-- instance Box_ PhysicsBox where
--     processPhoton b ph = []

-- data BoundaryBox = BoundaryBox {
--     radiance :: Number
--     } deriving (Show)
-- instance Box_ BoundaryBox where
--     processPhoton b ph = []

