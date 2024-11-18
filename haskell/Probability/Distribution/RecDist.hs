module Probability.Distribution.RecDist where

import Probability.Random
import Control.Monad.Fix

{- NOTE: Recursive distributions.

   We can *sample* from recursive distributions without actually creating a distribution object:

   > do
   >     ...
   >     rec let distFor node = case (parent tree node) of Just p -> normal (xs!p) (sigma^2); Nothing -> rootDist
   >         xs <- sample $ independent [ distFor node | node <- nodes ]
   >     ...

   This is equivalent to:

   > do
   >     ...
   >     distFunc xs = independent [distFor node | node <- nodes ] where
   >         distFor node = case (parent tree node) of Just p -> normal (xs!p) (sigma^2); Nothing -> rootDist
   >     xs <- sample $ recDist distFunc
   >     ...

   But its we can't *observe* from the distribution unless we specify the distribution function.

   > do
   >     ...
   >     distFunc xs = independent [distFor node | node <- nodes ] where
   >         distFor node = case (parent tree node) of Just p -> normal (xs!p) (sigma^2); Nothing -> rootDist
   >     observe xs (recDist distFunc)
   >     ...

   In PGMs, perhaps we could write something like

   > x ~ mkArray(nodes, node, normal( if node == root then rootMean else x[parent(node)], sigma^2))

   In strict PPLs, it might be possible to generate values at nodes of the tree by doing something like:

   > xRoot ~ rootDist
   > xsLeft ~ generate(leftSubtree, xRoot)
   > xsRight ~ generate(rightSubtree, xRoot)
   > xs = {root:xRoot} + xsLeft + xsRight

   This should work fine for purely sampling-based methods.  But for MCMC methods we often want to compute
   the prior density of the xs, and then it seems like this would not work.
-}

data RecDist d = RecDist (Result d->d)

instance Dist d => Dist (RecDist d) where
    type Result (RecDist d) = Result d

instance (Dist d, IOSampleable d) => IOSampleable (RecDist d) where
    sampleIO (RecDist f) = mfix (\value -> sampleIO (f value))

instance (Dist d, HasPdf d) => HasPdf (RecDist d) where
    pdf (RecDist f) x = pdf (f x) x

instance (Dist d, HasAnnotatedPdf d) => HasAnnotatedPdf (RecDist d) where
    type DistProperties (RecDist d) = DistProperties d
    annotated_densities (RecDist f) x = annotated_densities (f x) x

instance (Dist d, Sampleable d) => Sampleable (RecDist d) where
    sample (RecDist f) = mfix (\value -> sample (f value))

recDist = RecDist
