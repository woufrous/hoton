{-# LANGUAGE ExistentialQuantification #-}
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
    Face_(..),
    PhysicsBox(..),
    BoundaryBox(..),
) where

import Hoton.Types

data Photon = Photon deriving (Show)

class Show s => Source_ s

data Source = forall s. Source_ s => Source s
instance Source_ Source
instance Show Source where
    show (Source s) = show s

data SourceOrSink = SoSSource Source | Sink Number deriving (Show)

class Show f => Face_ f where

data Face = forall f. Face_ f => Face f
instance Show Face where
    show (Face f) = show f

data InteractionResult = IRPhoton Face Photon | IRSoS SourceOrSink deriving (Show)

class Show b => Box_ b where
    processPhoton :: b -> Photon -> [InteractionResult]

data Box = forall b. Box_ b => Box b
instance Box_ Box where
    processPhoton (Box b) = processPhoton b
instance Show Box where
    show (Box b) = show b

data PhysicsBox = PhysicsBox {
    height :: Number,
    beta :: Number
    } deriving (Show)
instance Box_ PhysicsBox where
    processPhoton b ph = []

data BoundaryBox = BoundaryBox {
    radiance :: Number
    } deriving (Show)
instance Box_ BoundaryBox where
    processPhoton b ph = []

