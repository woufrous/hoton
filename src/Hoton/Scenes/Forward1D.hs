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
    newDirection,
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

shiftPhoton innerFace leavesBox cbox ph
    | innerFace == FaceTop && leavesBox     = ph
    | innerFace == FaceTop && not leavesBox = Photon{dir=(dir ph), pos=((pos ph) `vadd` (ex `smul` (-c))), tau_r=(tau_r ph)}
    | innerFace == FaceBottom               = Photon{dir=(dir ph), pos=((pos ph) `vadd` (ex `smul` c))   , tau_r=(tau_r ph)}
    where
        c = cCenter cbox

cProcessInteractionResults :: (RandomGen g) => (Face Box1D) ->
                                               (ContainerBox1D) ->
                                               ([InteractionResult (Face Box1D)], g) ->
                                               ([InteractionResult (Face Box1D)], g)
cProcessInteractionResults _ _ ([], g) = ([], g)
cProcessInteractionResults innerFace cbox ((IRPhoton f ph):rem, g)
    | f == innerFace = cProcessInteractionResults (otherFace f) cbox $ processPhoton otherBox (shiftPhoton innerFace False cbox ph) g
    | otherwise      = ((IRPhoton f $ shiftPhoton innerFace True cbox ph):remProcessed, g')
    where
        (remProcessed, g')   = cProcessInteractionResults innerFace cbox (rem, g)
        ContainerBox1D b1 b2 = cbox
        thisBox              = if innerFace == FaceTop then b2 else b1
        otherBox             = if innerFace == FaceTop then b1 else b2

cProcessInteractionResults innerFace cbox ((IRSoS sos):rem, g) = ((IRSoS sos):remProcessed, g')
    where
        (remProcessed, g') = cProcessInteractionResults innerFace cbox (rem, g)

instance Box_ Box1D ContainerBox1D where
    getDim (ContainerBox1D btop bbot) = Height (h1+h2)
        where
            Height h1 = getDim btop
            Height h2 = getDim bbot
    addBox b other
        | l2 < l1   = containerBox1D b1 $ containerBox1D b2 other
        | otherwise = containerBox1D ((Box Box1D) b) other
        where
            ContainerBox1D b1 b2 = b
            BoxLevel1D l1        = boxLevel b1
            BoxLevel1D l2        = boxLevel b2
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
    getDim b   = Height (height b)
    addBox b   = containerBox1D ((Box Box1D) b)
    boxLevel b = BoxLevel1D 0
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
