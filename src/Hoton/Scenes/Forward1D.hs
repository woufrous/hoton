{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Hoton.Scenes.Forward1D
(
--    Source1D(..),
--    BoundaryBox1D(..),
    containerBox1D,
    Face(..),
    physicsBox1D,
    summarize1D,
    newDirection
) where

import Hoton.Types
import Hoton.Scene
import Hoton.Distributions
import Hoton.Vector
import Hoton.Matrix

-- data Source1D = SourceTop | SourceBottom deriving (Show)
-- instance Source_ Source1D


-- data BoundaryBox1D = BoundaryBox1D {
--     source :: Source1D
--    } deriving (Show)
--instance Box_ BoundaryBox1D where
--    processPhoton b ph = [IRSoS (SoSSource (Source (source b)))]

data Box1D = Box1D
data instance Face Box1D = FaceTop | FaceBottom deriving (Show,Eq)

--instance Box_ Box1D (Box Box1D)

data ContainerBox1D = ContainerBox1D (Box Box1D) (Box Box1D) deriving (Show)
instance Box_ Box1D ContainerBox1D where
--   processTopResults :: ContainerBox1D -> [InteractionResult] -> [InteractionResult]
--   processTopResults (ContainerBox1D _ _) [] = []
--   processTopResults (ContainerBox1D b1 b2) ((IRSoS sos):rs) =
--       ((IRSoS sos):(processTopResults (ContainerBox1D b1 b2) rs))
--   processTopResults (ContainerBox1D b1 b2) ((IRPhoton (Face FaceTop) ph):rs) =
--       ((IRPhoton (Face FaceTop) ph):(processTopResults (ContainerBox1D b1 b2) rs))
--   processBottonResults :: ContainerBox1D -> [InteractionResult] -> [InteractionResult]
    processPhoton (ContainerBox1D b1 b2) ph g = (results, g2)
        where
            (r1,g1) = processPhoton b1 ph g
            (r2,g2) = processPhoton b1 ph g1
            results = r1 ++ r2

newDirection :: Cartesian -> Number -> Number -> Cartesian
newDirection n mu phi = normalize $ mrotaxmu mu v' `mvmul` n
    where
        v' = normalize $ mrotax phi (normalize n) `mvmul` v
        v  = anyPerpendicular n

movePhotonZ :: Photon -> Number -> PhysicsBox1D -> Photon
movePhotonZ ph len_z b = Photon { pos=(pos ph) `vadd` ((dir ph) `smul` len),
                                  dir=(dir ph),
                                  tau_r=(tau_r ph) - len * (beta b) }
    where
        Cartesian _ _ dir_z = (dir ph)
        len                 = len_z / dir_z

data PhysicsBox1D = PhysicsBox1D {
    height :: Number,
    beta :: Number,
    scatterer :: RandomDistribution
    } deriving (Show)
instance Box_ Box1D PhysicsBox1D where
    processPhoton b ph g
        | z_scat < 0            = ([IRPhoton FaceBottom (movePhotonZ ph (0 -          z_start) b)], g)
        | z_scat > (height b)   = ([IRPhoton FaceTop    (movePhotonZ ph ((height b) - z_start) b)], g)
        | otherwise             = processPhoton b (Photon{pos=pos_scat,dir=dir_scat,tau_r=tau_new}) g'''
        where
            Cartesian _ _ z_start = pos ph
            pos_scat              = ((dir ph) `smul` ((tau_r ph)/(beta b))) `vadd` (pos ph)
            Cartesian _ _ z_scat  = pos_scat
            (tau_new, g')         = drawRandom ThicknessDistribution g
            (mu_scat, g'')        = drawRandom (scatterer b) g'
            (phi_scat, g''')      = drawRandom AzimutalDistribution g''
            dir_scat              = newDirection (dir ph) mu_scat phi_scat

containerBox1D b1 b2 = Box Box1D $ ContainerBox1D b1 b2
physicsBox1D h b s = Box Box1D $ PhysicsBox1D h b s

accIR (t,b) (IRPhoton FaceTop    _) = (t+1,b)
accIR (t,b) (IRPhoton FaceBottom _) = (t,b+1)

summarize1D :: [InteractionResult (Face Box1D)] -> (Number, Number)
summarize1D = foldl accIR (0,0)
