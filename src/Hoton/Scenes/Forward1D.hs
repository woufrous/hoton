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
    rayleighAtmos2Box,
    Dimensions(..)
) where

import System.Random

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
otherFace FaceTop    = FaceBottom
otherFace FaceBottom = FaceTop

data instance Dimensions Box1D = Height Number deriving (Show)
data instance BoxLevel   Box1D = BoxLevel1D Int deriving (Show)


--instance Box_ Box1D (Box Box1D)

data ContainerBox1D = ContainerBox1D (Box Box1D) (Box Box1D) deriving (Show)
cCenter (ContainerBox1D    _ bbot) = h
    where
        Height h = getDim bbot

ex = Cartesian 0 0 1

-- changing between boxes involves coordinate system transformations
shiftPhoton innerFace leavesBox cbox ph
    | innerFace == FaceTop && leavesBox     = ph
    | innerFace == FaceTop && not leavesBox = movePhotonV ph $ ex `smul` (-c)
    | innerFace == FaceBottom               = movePhotonV ph $ ex `smul` c
    where
        c = cCenter cbox

cProcessInteractionResults :: (Face Box1D) ->
                              (ContainerBox1D) ->
                              ([InteractionResult (Face Box1D)], [Number]) ->
                              ([InteractionResult (Face Box1D)], [Number])
-- easy: no photons -> no processing
cProcessInteractionResults _ _ ([], g) = ([], g)
-- easy as well: soruces or sinks are not processed
cProcessInteractionResults innerFace cbox ((IRSoS sos):rem, g) = ((IRSoS sos):remProcessed, g')
    where
        (remProcessed, g') = cProcessInteractionResults innerFace cbox (rem, g)
-- actual photons must be either exchanged or propagated further out of the box
cProcessInteractionResults innerFace cbox ((IRPhoton f ph):rem, g)
    | leavesBox = ((IRPhoton f ph'):remProcessedL, gL)
    | otherwise = (remProcessedS ++ remProcessedS', gS')
    where
        leavesBox             = f /= innerFace
        ph'                   = shiftPhoton innerFace leavesBox cbox ph
        -- leaves Box:
        (remProcessedL, gL)   = cProcessInteractionResults innerFace cbox (rem, g)
        -- stays in Box:
        (remProcessedS , gS ) = cProcessInteractionResults (otherFace f) cbox $ processPhoton otherBox ph' g
        (remProcessedS', gS') = cProcessInteractionResults innerFace cbox (rem, gS)
        -- helpers
        ContainerBox1D b1 b2  = cbox
        thisBox               = if innerFace == FaceTop then b2 else b1
        otherBox              = if innerFace == FaceTop then b1 else b2

instance Box_ Box1D ContainerBox1D where
    getDim (ContainerBox1D btop bbot) = Height (h1+h2)
        where
            Height h1 = getDim btop
            Height h2 = getDim bbot
    -- adds bot on TOP
    addBox b other
        | ltop < lbot   = containerBox1D (containerBox1D other btop) bbot
        | otherwise = containerBox1D other ((Box Box1D) b)
        where
            ContainerBox1D btop bbot = b
            BoxLevel1D ltop          = boxLevel btop
            BoxLevel1D lbot          = boxLevel bbot
    boxLevel b = BoxLevel1D $ max l1 l2
        where
            ContainerBox1D b1 b2 = b
            BoxLevel1D l1        = boxLevel b1
            BoxLevel1D l2        = boxLevel b2
    processPhoton c ph g
        | z_start <= cCenter c    = cProcessInteractionResults FaceTop    c $ processPhoton bbot ph g
        | otherwise               = cProcessInteractionResults FaceBottom c $ processPhoton btop (shiftPhoton FaceTop False c ph) g
        where
            ContainerBox1D btop bbot = c
            Cartesian _ _ z_start = pos ph

data PhysicsBox1D = PhysicsBox1D {
    height :: Number,
    scatterers :: [Scatterer]
    } deriving (Show)
betaAbsTotal :: PhysicsBox1D -> Number
betaAbsTotal b = sum $ map betaAbs $ scatterers b
instance Box_ Box1D PhysicsBox1D where
    getDim b        = Height (height b)
    addBox b other  = containerBox1D other ((Box Box1D) b)
    boxLevel b      = BoxLevel1D 0
    processPhoton b ph g
        | z_scat < 0            = ([IRPhoton FaceBottom (movePhotonZ ph sc dz_to_bottom)], g)
        | z_scat > (height b)   = ([IRPhoton FaceTop    (movePhotonZ ph sc dz_to_top   )], g)
        | otherwise             = processPhoton b scPh g'
        where
            sc                    = head $ scatterers b
            Cartesian _ _ z_start = pos ph
            pos_scat              = posScat ph sc
            Cartesian _ _ z_scat  = pos_scat
            dz_to_bottom          = 0 -          z_start
            dz_to_top             = (height b) - z_start
            (scPh, g')            = scatteredPhoton ph sc g

containerBox1D b1 b2 = Box Box1D $ ContainerBox1D b1 b2
physicsBox1D h b s = Box Box1D $ PhysicsBox1D h [Scatterer 0.0 b s]

accIR (t,b) (IRPhoton FaceTop    _) = (t+1,b)
accIR (t,b) (IRPhoton FaceBottom _) = (t,b+1)

summarize1D :: [InteractionResult (Face Box1D)] -> (Number, Number)
summarize1D = foldl accIR (0,0)


rayleighAtmos2BoxList :: [(Double,Double,Double)] -> [(Box Box1D)]
rayleighAtmos2BoxList [a]       = []
rayleighAtmos2BoxList (a:b:res) = (physicsBox1D (zhigh-zlow) beta_sca (RandomDistribution Rayleigh)):rayleighAtmos2BoxList (b:res)
    where
        (zlow, beta_sca, _) = a
        (zhigh,       _, _) = b

rayleighAtmos2Box' :: [(Box Box1D)] -> Maybe (Box Box1D)
rayleighAtmos2Box' [] = Nothing
rayleighAtmos2Box' l  = Just $ foldl1 addBox l

rayleighAtmos2Box = rayleighAtmos2Box' . rayleighAtmos2BoxList
