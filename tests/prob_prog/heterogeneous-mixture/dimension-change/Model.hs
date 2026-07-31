{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Eq ((/=))
import Data.Function (($))
import Data.OldList (filter, length)
import Data.String (String)
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.Laplace (laplace)
import Probability.Distribution.List (iid)
import Probability.Distribution.Mixture
import Probability.Random ((%=%), prior)

-- Sample several spike-and-slab coefficients so categorical moves can change
-- both the active component labels and their total cardinality.
model = do
    coefficients <- prior $ iid 12
        ((1 / 2) .*. delta 0 |+| (1 / 2) .*. laplace 0 1)
    return [("active" :: String) %=% length (filter (/= 0) coefficients)]

main _ = return model
