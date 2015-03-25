module Hoton.TestUtils
(
    approx
) where

-- p: Number of decimal places
-- s: Should-Value
-- i: Is-Value
approx p s i = abs (s-i) < 0.1^p

