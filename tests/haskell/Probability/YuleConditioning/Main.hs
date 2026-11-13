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
import Probability.Distribution.Tree.Modifiable (conditionedModifiableTimeTree)
import Probability.Distribution.Tree.BirthDeath
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
    let changed = conditionedModifiableTimeTree Crown (AtAge 1) id three
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
        bdTree <- sample $ birthDeath [(),()] 2 2 1 Crown (AtAge 1) Unconditioned
        observe 0 $ normal (nodeTime bdTree (root bdTree)) 1
        return []
    prior <- runContextAction logPrior state
    print $ close prior (-4 + log (1/9))

    -- Tree-rate wrappers must not affect a density on node ages. Concrete pdf tests above
    -- cannot catch representation restrictions or accidental use of substitution lengths.
    let rated = rate_time_tree three (IntMap.fromSet (\_ -> 7) (getUEdgesSet three))
    print $ yulePrFactors 3 2 Origin Flat (Tips 3) rated
         == yulePrFactors 3 2 Origin Flat (Tips 3) three

    -- Extinction changes normalization and survival even on fixed-size trees. Compare the Yule
    -- limit and independent critical/subcritical values; smoke tests cannot detect these errors.
    -- Retain while both distributions implement the same conditioning choices.
    let choices = [(Origin,AtAge 1,Unconditioned), (Origin,AtAge 1,Surviving),
                   (Origin,AtAge 1,Tips 3), (Crown,AtAge 0.5,Unconditioned),
                   (Crown,AtAge 0.5,Tips 3), (Origin,Flat,Tips 3)]
        bd l m start age cond = pdf (birthDeath [(),(),()] l m 1 start age cond) three
    print $ all id [close (ln $ bd 2 0 start age cond) (p3 start age cond)
                   | (start,age,cond) <- choices]
    print $ all id [close (ln $ bd 2 2 start age cond) (log expected)
                   | ((start,age,cond),expected) <- zip choices [8/243,8/81,2/3,4/27,16/27,4/9]]
    print $ all id [close (ln $ bd 1 2 start age cond) (log expected)
                   | ((start,age,cond),expected) <- zip choices
                       [0.015018309751093653,0.06662968723004067,0.7249777031752218,
                        0.17929461704044652,0.6164794596308396,0.568010805389819]]
    print $ all id
        [ pdf (birthDeath [()] 1 2 1 Origin Flat (Tips 1)) singleton == 1
        , pdf (birthDeath [()] 1 2 1 Origin (AtAge 1) (Tips 1)) singleton == 1
        , pdf (birthDeath [(),()] 1 2 1 Crown (AtAge 0.5) (Tips 2)) two == 1
        , bd 1 (-1) Origin Flat (Tips 3) == 0
        , bd 0 1 Origin Flat (Tips 3) == 0
        , bd 1 2 Origin (AtAge 0.1) (Tips 3) == 0
        , bd 1 2 Crown (AtAge 1) (Tips 3) == 0
        ]
    print $ all id [close (birthDeathHazard l m 1 (birthDeathInverseHazard l m 1 h)) h
                   | (l,m) <- [(2,0),(2,2),(1,1+1e-12),(1,2)], h <- [0,1e-10,0.5,30]]
    d <- runRandomStrict $ sample $ birthDeath [(),(),()] 1 2 1 Origin Flat (Tips 3)
    e <- runRandomStrict $ sample $ birthDeath [(),(),()] 2 2 1 Crown (AtAge 1) (Tips 3)
    f <- runRandomStrict $ sample $ birthDeath [(),(),()] 2 1 1 Origin (AtAge 1) (Tips 3)
    print $ numLeaves d == 3 && pdf (birthDeath [(),(),()] 1 2 1 Origin Flat (Tips 3)) d > 0
         && nodeTime e (root e) == 1 && pdf (birthDeath [(),(),()] 2 2 1 Crown (AtAge 1) (Tips 3)) e > 0
         && nodeTime f (root f) <= 1 && pdf (birthDeath [(),(),()] 2 1 1 Origin (AtAge 1) (Tips 3)) f > 0

    -- Sampling fraction changes branching ages and count normalizers, not just an overall factor.
    -- Independent log densities cover critical, subcritical, and supercritical laws; keep these
    -- alongside the rho=1 checks while the same constructor supports incomplete sampling.
    let rhoCases = [(2,2,[-5.0756924307295597,-3.1297822816742462,-0.28778410806229748,
                          -1.8770193131788784,-0.41376391092285947,-1.8770193131788784]),
                    (1,2,[-7.459955321749896,-4.731469297081258,-0.2380988447352817,
                          -2.5029832724126204,-0.42668762752329814,-0.50604088553849402]),
                    (2,0.5,[-3.2863946735017473,-2.5202399724769768,-0.40161300888941964,
                            -1.6972324520121516,-0.39710464470244172,-1.0256327895005244])]
    print $ all id [close (ln $ pdf (birthDeath [(),(),()] l m 0.2 start age cond) three) expected
                   | (l,m,values) <- rhoCases, ((start,age,cond),expected) <- zip choices values]
    print $ all id [close (birthDeathHazard l m rho (birthDeathInverseHazard l m rho h)) h
                   | (l,m) <- [(2,0),(2,2),(1,1+1e-12),(1,2)], rho <- [0.05,0.5,1],
                     h <- [0,1e-10,0.5,30,600]]
    print $ all id
        [ pdf (birthDeath [()] 1 2 0.2 Origin Flat (Tips 1)) singleton == 1
        , pdf (birthDeath [(),()] 1 2 0.2 Crown (AtAge 0.5) (Tips 2)) two == 1
        , pdf (birthDeath [(),(),()] 1 2 0 Origin Flat (Tips 3)) three == 0
        , pdf (birthDeath [(),(),()] 1 2 1.1 Origin Flat (Tips 3)) three == 0 ]
    g <- runRandomStrict $ sample $ birthDeath [(),(),()] 1 2 0.05 Origin Flat (Tips 3)
    h <- runRandomStrict $ sample $ birthDeath [(),(),()] 2 0 0.05 Crown (AtAge 1) (Tips 3)
    print $ pdf (birthDeath [(),(),()] 1 2 0.05 Origin Flat (Tips 3)) g > 0
         && nodeTime h (root h) == 1 && pdf (birthDeath [(),(),()] 2 0 0.05 Crown (AtAge 1) (Tips 3)) h > 0
