module Hoton.Debug
( _traceShowId,
  _breakIfVectorIsNan
) where

import Hoton.Vector

import Debug.Trace

_traceShowId a = traceShow a a

_breakIfVectorIsNan :: Cartesian -> Cartesian
_breakIfVectorIsNan (Cartesian x1 x2 x3)
    | isNaN x1 || isNaN x2 || isNaN x3    = error("NAN reached")
    | x1 == 0.0 && x2 == 0.0 && x3 == 0.0 = error("0.0 reached")
    | otherwise                           = Cartesian x1 x2 x3

