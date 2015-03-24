module Hoton.Scenes.Forward1D
(
--    Source1D(..),
--    BoundaryBox1D(..),
--    ContainerBox1D(..),
    Face1D(..),
    PhysicsBox1D(..)
) where

import Hoton.Types
import Hoton.Scene
import Hoton.Distributions

-- data Source1D = SourceTop | SourceBottom deriving (Show)
-- instance Source_ Source1D

data Face1D = FaceTop | FaceBottom deriving (Show)
instance Face_ Face1D

-- data BoundaryBox1D = BoundaryBox1D {
--     source :: Source1D
--    } deriving (Show)
--instance Box_ BoundaryBox1D where
--    processPhoton b ph = [IRSoS (SoSSource (Source (source b)))]

-- data ContainerBox1D = ContainerBox1D Box Box deriving (Show)
-- instance Box_ ContainerBox1D where
--    processTopResults :: ContainerBox1D -> [InteractionResult] -> [InteractionResult]
--    processTopResults (ContainerBox1D _ _) [] = []
--    processTopResults (ContainerBox1D b1 b2) ((IRSoS sos):rs) =
--        ((IRSoS sos):(processTopResults (ContainerBox1D b1 b2) rs))
--    processTopResults (ContainerBox1D b1 b2) ((IRPhoton (Face FaceTop) ph):rs) =
--        ((IRPhoton (Face FaceTop) ph):(processTopResults (ContainerBox1D b1 b2) rs))
--    processBottonResults :: ContainerBox1D -> [InteractionResult] -> [InteractionResult]
--    processPhoton (ContainerBox1D b1 b2) ph = results
--        where
--            results = (processPhoton b1 ph) ++ (processPhoton b2 ph)

data PhysicsBox1D = PhysicsBox1D {
    height :: Number,
    beta :: Number,
    scatterer :: RandomDistribution
    } deriving (Show)
instance Box_ PhysicsBox1D where
    processPhoton b ph = [IRPhoton (Face FaceTop) ph]


