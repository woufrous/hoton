{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Hoton.Scene
(
    Photon(..),
    Source(..),
    Source_(..),
    SourceOrSink(..),
    InteractionResult(..),
    Box(..),
    Box_(..),
    Face(..),
    --PhysicsBox(..),
    --BoundaryBox(..),
) where

import Hoton.Types
import Hoton.Vector
import Hoton.Distributions

import System.Random

data Photon = Photon {
    tau_r :: Number,
    pos :: Cartesian,
    dir :: Cartesian
} deriving (Show)

class Show s => Source_ s

data Source = forall s. Source_ s => Source s
instance Source_ Source
instance Show Source where
    show (Source s) = show s

data SourceOrSink = SoSSource Source | Sink Number deriving (Show)

data family Face boxType

data InteractionResult faceType = IRPhoton faceType Photon | IRSoS SourceOrSink deriving (Show)

data Box bFamily = forall b. Box_ bFamily b => Box bFamily b

instance Show (Box bFamily) where
    show (Box _ b) = show b

class Show b => Box_ bFamily b where
    processPhoton :: (RandomGen g) => b -> Photon -> g -> ([InteractionResult (Face bFamily)], g)
    processManyEqualPhotons :: (RandomGen g) => b -> Photon -> g -> [InteractionResult (Face bFamily)]
    processManyEqualPhotons b ph g = res' ++ processManyEqualPhotons b ph_new  g''
        where
            (res', g')       = processPhoton b ph g
            (tau_r_new, g'') = drawRandom ThicknessDistribution g'
            ph_new           = Photon tau_r_new (pos ph) (dir ph)

instance Box_ bFamily (Box bFamily) where
    processPhoton (Box _ b) ph g = processPhoton b ph g

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

