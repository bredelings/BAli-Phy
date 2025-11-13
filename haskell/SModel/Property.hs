module SModel.Property where

import Foreign.Vector (EVector, toVector)
import Probability.Distribution.Discrete (values)
import SModel.Rate (rate)
import Markov
import qualified Data.JSON as J    

data StateProperties a = StateProperties [a]

data ComponentStateProperties a = ComponentStateProperties [StateProperties a]

getPropertyComponents (ComponentStateProperties sps) = length sps
getPropertyStatesForComponent (ComponentStateProperties csps) c = length sps
    where StateProperties sps = csps!!c
getPropertyForComponentState (ComponentStateProperties csps) c s = sps!!s
    where StateProperties sps = csps!!c

instance Functor StateProperties where
    fmap f (StateProperties ps) = StateProperties (fmap f ps)

instance Functor ComponentStateProperties where
    fmap f (ComponentStateProperties csps) = ComponentStateProperties (fmap (fmap f) csps)

type Property = ComponentStateProperties Double

{- Suppose we use a function plus extents
   Then each component could have a different number of states.
   So we'd have ComponentStateProperties Int (Int -> Int) (Int -> Int -> Double)
 -}

{- QUESTION: Which rate do we care about here?
   On a DNA model, its simple, but in other cases not so much.
   * for codons, we could care about the DNA rate or the amino-acid rate
   * for markov-modulated models, we could care about the within-model rate but not the between model rate.

   If we had a mixture of modulated and non-modulated models, then we'd need a different rate function.
   One way around that would be to map each state back to a sub-state, and calculate the rate on the sub-state.
   Then each mixture component would use that.
   I guess that's what the smap is for, maybe? -}

rateProperty dist = ComponentStateProperties [StateProperties $ replicate (getNStates m) (rate m) | m <- values dist]

markovModulateProperty (ComponentStateProperties csps) = StateProperties $ concat [ ps | StateProperties ps <- csps]

instance J.ToJSON a => J.ToJSON (ComponentStateProperties a) where
    toJSON (ComponentStateProperties csps) = J.toJSON [ sps | StateProperties sps <- csps]
    toEncoding (ComponentStateProperties csps) = J.toEncoding [ sps | StateProperties sps <- csps]
