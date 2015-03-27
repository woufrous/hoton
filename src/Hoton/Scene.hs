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
    initializePhoton,
    Scatterer(..),
    movePhotonZ,
    posScat,
    movePhotonV,
    newDirection,
    scatteredPhoton,
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
import Hoton.Matrix
import Hoton.Distributions

import System.Random

data Photon = Photon {
    tau_r :: !Number,
    pos :: !Cartesian,
    dir :: !Cartesian,
    weight :: !Number
} deriving (Show, Eq)

initializePhoton :: Cartesian -> Cartesian -> [Number] -> (Photon, [Number])
initializePhoton pos0 dir0 g = (Photon tau0 pos0 dir0 1, g')
    where
        (tau0, g') = drawRandom ThicknessDistribution g

posScat :: Photon -> Scatterer -> Cartesian
posScat ph sc = ((dir ph) `smul` ((tau_r ph)/(betaScat sc))) `vadd` (pos ph)

movePhotonV :: Photon -> Cartesian -> Photon
movePhotonV ph v = Photon{dir=(dir ph), pos=((pos ph) `vadd` v), tau_r=(tau_r ph), weight=(weight ph)}

data Scatterer = Scatterer {
    betaAbs  :: Number,
    betaScat :: Number,
    phaseDistribution :: RandomDistribution
} deriving (Show)

movePhotonZ :: Photon -> Scatterer -> Number -> Photon
movePhotonZ ph sc len_z = Photon { pos=(pos ph) `vadd` ((dir ph) `smul` len),
                                   dir=(dir ph),
                                   tau_r=(tau_r ph) - (len * (betaScat sc)),
                                   weight=(weight ph) }
    where
        Cartesian _ _ dir_z = (dir ph)
        len                 = len_z / dir_z

reducePhotonWeight :: Photon -> Number -> Photon
reducePhotonWeight ph f = Photon { pos=(pos ph),
                                   dir=(dir ph),
                                   tau_r=(tau_r ph),
                                   weight=f*(weight ph) }

newDirection :: Cartesian -> Number -> Number -> Cartesian
newDirection n mu phi = normalize $ mrotaxmu mu v' `mvmul` n
    where
        v' = normalize $ mrotax phi (normalize n) `mvmul` v
        v  = anyPerpendicular n

scatteredPhoton :: Photon -> Scatterer -> [Number] -> (Photon, [Number])
scatteredPhoton ph sc g = (Photon{pos=pos_scat,dir=dir_scat,tau_r=tau_new,weight=(weight ph)}, g''')
    where
        pos_scat              = posScat ph sc
        (tau_new, g')         = drawRandom ThicknessDistribution g
        (mu_scat, g'')        = drawRandom (phaseDistribution sc) g'
        (phi_scat, g''')      = drawRandom AzimutalDistribution g''
        dir_scat              = newDirection (dir ph) mu_scat phi_scat


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
            ph_new           = Photon tau_r_new (pos ph) (dir ph) (weight ph)
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

