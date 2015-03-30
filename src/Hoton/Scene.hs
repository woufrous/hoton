{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Hoton.Scene
(
    Photon(..),
    initializePhoton,
    initializePhotonWithTau,
    remainingS,
    photonWeight,
    posScat,
    movePhotonV,
    travelPhotonZ,
    scatteredPhoton,
    Scatterer(..),
    newDirection,
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
    tauR   :: !Number,
    pos    :: !Cartesian,
    dir    :: !Cartesian,
    tauAbs :: !Number
}

remainingS :: Photon -> Scatterer -> Number
remainingS ph sc = tauR ph / betaScat sc

photonWeight :: Photon -> Number
photonWeight ph  = exp(-(tauAbs ph))

posScat :: Photon -> Scatterer -> Cartesian
posScat ph sc    = (dir ph `smul` remainingS ph sc)  `vadd` pos ph

movePhotonV :: Photon -> Cartesian -> Photon
movePhotonV ph v = ph{pos = pos ph `vadd` v}
travelPhotonZ :: Photon -> Scatterer -> Number -> Number -> Photon
travelPhotonZ ph sc betaAbsTot len_z = ph'
        where
            Cartesian _ _ dir_z = dir ph
            len                 = len_z / dir_z
            ph' = ph {pos    = pos ph `vadd` (dir ph `smul` len),
                      tauR   = tauR ph - (len * betaScat sc),
                      tauAbs = (betaAbsTot * abs len) + tauAbs ph}

instance Show Photon where
    show ph = "Photon"

initializePhoton :: Cartesian -> Cartesian -> [Number] -> (Photon, [Number])
initializePhoton pos0 dir0 g = (ph', g')
    where
        (tau0, g') = drawRandom ThicknessDistribution g
        ph'        = Photon tau0 pos0 dir0 0

initializePhotonWithTau :: Number -> Cartesian -> Cartesian -> Photon
initializePhotonWithTau tau0 pos0 dir0 = ph'
    where
        ph' = Photon tau0 pos0 dir0 0

scatteredPhoton :: Photon -> Scatterer -> Number -> [Number] -> (Photon, [Number])
scatteredPhoton ph sc betaAbsTot g = (ph', g''')
        where
            pos_scat         = posScat ph sc
            (tau_new, g')    = drawRandom ThicknessDistribution g
            (mu_scat, g'')   = drawRandom (phaseDistribution sc) g'
            (phi_scat, g''') = drawRandom AzimutalDistribution g''
            dir_scat         = newDirection (dir ph) mu_scat phi_scat
            tauAbs'          = (betaAbsTot * remainingS ph sc) + tauAbs ph
            ph'              = ph{pos=pos_scat,
                                  dir=dir_scat,
                                  tauR=tau_new,
                                  tauAbs=tauAbs'}

data Scatterer = Scatterer {
    betaAbs  :: Number,
    betaScat :: Number,
    phaseDistribution :: RandomDistribution
} deriving (Show)


newDirection :: Cartesian -> Number -> Number -> Cartesian
newDirection n mu phi = normalize $ mrotaxmu mu v' `mvmul` n
    where
        v' = normalize $ mrotax phi (normalize n) `mvmul` v
        v  = anyPerpendicular n

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
            (res', r')     = processPhoton b ph r
            (tau_new, r'') = drawRandom ThicknessDistribution r'
            ph_new         = ph{tauR=tau_new}
    getDim :: b -> Dimensions bFamily
    addBox :: b -> Box bFamily -> Box bFamily
    boxLevel :: b -> BoxLevel bFamily

instance Box_ bFamily (Box bFamily) where
    processPhoton (Box _ b) = processPhoton b
    getDim        (Box _ b) = getDim b
    addBox        (Box _ b) = addBox b
    boxLevel      (Box _ b) = boxLevel b

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

