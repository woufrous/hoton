module Hoton.TestUtils
(
    approx,
    shouldBeApprox
) where

import Test.Hspec

-- s: Should-Value
-- i: Is-Value
approx s i = abs (s-i) < 0.1^8
a `shouldBeApprox` b = a `shouldSatisfy` approx b
