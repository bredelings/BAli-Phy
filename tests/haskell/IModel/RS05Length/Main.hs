{-# LANGUAGE NoImplicitPrelude #-}
import Compiler.Floating (exp, expm1, log)
import Compiler.Fractional
import Compiler.Integral ((^))
import Numeric.Log (ln)
import Compiler.Num
import Data.Bool
import Data.Function (($))
import Data.Ord
import Data.Tuple (snd)
import IModel
import System.IO (print)

near x y = abs (ln x - ln y) < 1e-10

-- Initial-likelihood tests do not check the length law at mean length one or the rate transform.
-- Retain these analytic checks while RS05 uses this HMM and its closed-form length probabilities.
main = do
  let tau = 0.001
      delta = 0.02
      p = tau / (1 - (1-tau)*delta)
      s = (1-tau)*(1-delta) / (1 - (1-tau)*delta)
      lengthPr = rs05_lengthp (rs05_branch_HMM 0 delta tau 1 False)
      zeroPr = rs05_lengthp (rs05_branch_HMM 0 0 tau 1 False)
      transformed x = snd (rs05 x 3 tau ())
      direct x = rs05_lengthp (rs05_branch_HMM (2/3) (event/(1+event)) tau 1 False)
        where event = -expm1 (-2*exp x)
  print [near (lengthPr n) (p * s^n) | n <- [0,1,2,10,100]]
  print [near (zeroPr n) (tau * (1-tau)^n) | n <- [0,1,2,10,100]]
  print [near (transformed x n) (direct x n) | x <- [-1000,-20,-4,10], n <- [0,1,10]]
