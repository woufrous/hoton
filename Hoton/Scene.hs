{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Hoton.Scene
(
    Photon(..),
    Source(..),
    Source_(..),
    SourceOrSink(..),
    InteractionResult(..),
    Box(..),
    Face(..),
    --PhysicsBox(..),
    --BoundaryBox(..),
) where

import Hoton.Types
import Hoton.Vector

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

class Box b where
    processPhoton :: (RandomGen g) => b -> Photon -> g -> ([InteractionResult (Face b)], g)

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

