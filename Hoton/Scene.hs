{-# LANGUAGE ExistentialQuantification #-}
module Hoton.Scene
() where

import Hoton.Types

data Photon = Photon deriving (Show)

data SourceOrSink = Source Number | Sink Number deriving (Show)

data InteractionResult = IRPhoton Photon | IRSoS SourceOrSink deriving (Show)

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
    processPhoton b ph = [IRPhoton ph]

data BoundaryBox = BoundaryBox {
    radiance :: Number
    } deriving (Show)
instance Box_ BoundaryBox where
    processPhoton b ph = [IRSoS (Source (radiance b))]

data ContainerBox = ContainerBox Box Box deriving (Show)
instance Box_ ContainerBox where
    processPhoton (ContainerBox b1 b2) ph = (processPhoton b1 ph) ++ (processPhoton b2 ph)

