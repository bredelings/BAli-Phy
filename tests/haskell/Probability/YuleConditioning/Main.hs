{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Num
import Compiler.Fractional
import Compiler.Floating
import Data.Bool
import Data.Eq
import Data.Ord
import Data.Function (($), id)
import Data.List (zip, all)
import Data.Foldable (length)
import qualified Data.IntMap as IntMap
import Probability.Dist (HasPdf(pdf))
import Probability.Random (sample, runRandomStrict, makeMCMCState, observe)
import Probability.Distribution.Normal (normal)
import MCMC (runContextAction, logPrior)
import Control.Monad (return)
import Probability.Distribution.Tree.Yule
import Probability.Distribution.Tree.Conditioning
import Numeric.Pow (ln)
import Tree
import System.IO (print)

close x y = abs (x-y) < 1e-10

-- Smoke tests cannot detect missing rate-dependent normalizers. Small labelled trees check
-- the count factors, topology coefficient, singleton support, and fixed crown representation.
-- Retain these while the conditioning interface distinguishes count-retaining and Tips laws.
main = do
    let two = time_tree (addRoot 2 $ treeFromEdges [0,1,2] [(0,2),(1,2)])
                        (IntMap.fromList [(0,0),(1,0),(2,0.5)])
        three = time_tree (addRoot 4 $ treeFromEdges [0,1,2,3,4] [(0,3),(1,3),(3,4),(2,4)])
                          (IntMap.fromList [(0,0),(1,0),(2,0),(3,0.25),(4,0.5)])
        singleton = time_tree (addRoot 0 $ treeFromEdges [0] []) (IntMap.fromList [(0,0)])
        p2 start age cond = ln $ pdf (yule [(),()] 2 start age cond) two
        p3 start age cond = ln $ pdf (yule [(),(),()] 2 start age cond) three
    print $ all id
        [ close (p2 Origin Flat (Tips 2)) (log 4 - 2)
        , close (p2 Origin (AtAge 1) Unconditioned) (log 2 - 3)
        , close (p2 Origin (AtAge 1) Surviving) (p2 Origin (AtAge 1) Unconditioned)
        , close (p2 Origin (AtAge 1) (Tips 2)) (log 2 - 1 - log1mexp (-2))
        , close (p2 Crown (AtAge 0.5) Unconditioned) (-2)
        , close (p2 Crown (AtAge 0.5) (Tips 2)) 0
        , close (p3 Origin Flat (Tips 3)) (log 8 - 2.5)
        , close (p3 Crown (AtAge 0.5) (Tips 3)) (log (2/3) - 0.5 - log1mexp (-1))
        , close (ln $ pdf (yule [()] 2 Origin (AtAge 1) Unconditioned) singleton) (-2)
        , close (ln $ pdf (yule [()] 2 Origin (AtAge 1) (Tips 1)) singleton) 0
        , close (ln $ pdf (yule [()] 2 Origin Flat (Tips 1)) singleton) 0
        , pdf (yule [(),()] (-1) Origin Flat (Tips 2)) two == 0
        , pdf (yule [(),()] 2 Crown (AtAge (-1)) (Tips 2)) two == 0
        , pdf (yule [(),()] 2 Origin (AtAge 0.25) (Tips 2)) two == 0
        , pdf (yule [(),()] 2 Crown (AtAge 1) (Tips 2)) two == 0
        ]
    let changed = yuleModifiableTimeTree Crown (AtAge 1) id three
    print $ nodeTime changed (root changed) == 1
    a <- runRandomStrict $ sample $ yule [(),(),()] 2 Origin (AtAge 1) (Tips 3)
    b <- runRandomStrict $ sample $ yule [(),(),()] 2 Crown (AtAge 1) Unconditioned
    c <- runRandomStrict $ sample $ yule [()] 2 Origin Flat (Tips 1)
    print $ numLeaves a == 3 && nodeTime a (root a) <= 1
         && numLeaves b == 3 && nodeTime b (root b) == 1
         && numLeaves c == 1 && nodeTime c (root c) == 0

    -- A fixed root must still register the count probability; pdf checks cannot test effects.
    state <- makeMCMCState $ do
        tree <- sample $ yule [(),()] 2 Crown (AtAge 1) Unconditioned
        observe 0 $ normal (nodeTime tree (root tree)) 1
        return []
    prior <- runContextAction logPrior state
    print $ close prior (-4)

    -- Tree-rate wrappers must not affect a density on node ages. Concrete pdf tests above
    -- cannot catch representation restrictions or accidental use of substitution lengths.
    let rated = rate_time_tree three (IntMap.fromSet (\_ -> 7) (getUEdgesSet three))
    print $ yulePrFactors 3 2 Origin Flat (Tips 3) rated
         == yulePrFactors 3 2 Origin Flat (Tips 3) three
