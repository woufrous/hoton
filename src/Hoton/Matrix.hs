module Hoton.Matrix
(
    Matrix3D (..),
    mgen,
    meye,
    mvmul,
    mrotx,
    mroty,
    mrotz,
    mrotaxmu,
    mrotax
) where

import Hoton.Types
import Hoton.Vector
import Text.Printf

data Matrix3D = Matrix3D Cartesian Cartesian Cartesian deriving (Eq)
instance Show Matrix3D where
    show (Matrix3D (Cartesian x11 x12 x13) (Cartesian x21 x22 x23) (Cartesian x31 x32 x33)) = formatted
        where
        formatted = printf "[[%6.3f %6.3f %6.3f]\n [%6.3f %6.3f %6.3f]\n [%6.3f %6.3f %6.3f]]" x11 x12 x13 x21 x22 x23 x31 x32 x33
mgen :: ((Number, Number, Number), (Number, Number, Number), (Number, Number, Number)) -> Matrix3D
mgen ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) =
    Matrix3D (Cartesian x11 x12 x13) (Cartesian x21 x22 x23) (Cartesian x31 x32 x33)
meye = mgen ((1,0,0),(0,1,0),(0,0,1))
mvmul :: Matrix3D -> Cartesian -> Cartesian
mvmul (Matrix3D x1 x2 x3) c = Cartesian (c `scalar` x1) (c `scalar` x2) (c `scalar` x3)

mrotx :: Number -> Matrix3D
mrotx alpha = mgen ((1,0,0), (0,(cos alpha),(-sin alpha)), (0,(sin alpha),(cos alpha)))
mroty :: Number -> Matrix3D
mroty alpha = mgen (((cos alpha),0,(sin alpha)), (0,1,0), ((-sin alpha),0,(cos alpha)))
mrotz :: Number -> Matrix3D
mrotz alpha = mgen (((cos alpha),(-sin alpha),0), ((sin alpha),(cos alpha),0), (0,0,1))
mrotaxmu :: Number -> Cartesian -> Matrix3D
mrotaxmu mu (Cartesian n1 n2 n3) = mgen (
        ( (n1*n1*(1-mu)+   mu), (n1*n2*(1-mu)-n3*sa), (n1*n3*(1-mu)+n2*sa) ),
        ( (n2*n1*(1-mu)+n3*sa), (n2*n2*(1-mu)+   mu), (n2*n3*(1-mu)-n1*sa) ),
        ( (n3*n1*(1-mu)-n2*sa), (n3*n2*(1-mu)+n1*sa), (n3*n3*(1-mu)+   mu) ))
    where
        sa = sqrt (1 - (mu*mu))

mrotax :: Number -> Cartesian -> Matrix3D
mrotax alpha = mrotaxmu $ cos alpha
