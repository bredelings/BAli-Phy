module Probability.Distribution.PhyloCTMC.FixedA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.FixedA.Properties,
     module Probability.Distribution.PhyloCTMC.FixedA.Reversible,
     module Probability.Distribution.PhyloCTMC.FixedA.NonReversible,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC,
     VariablePhyloCTMC(..), variable
    )

where

import Probability.Distribution.PhyloCTMC.FixedA.Reversible
import Probability.Distribution.PhyloCTMC.FixedA.NonReversible
import Probability.Distribution.PhyloCTMC.FixedA.Sample
import Probability.Distribution.PhyloCTMC.FixedA.Properties
import Probability.Distribution.PhyloCTMC.Properties
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.FixedA
import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Array
import Data.Matrix
import Data.Foldable
import Foreign.Maybe
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (fromJust)
import SModel.Likelihood.CLV (CondLikes)

----------------------

data VariablePhyloCTMC t s r = Variable (PhyloCTMC t Int s r)

variable = Variable

{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotated_subst_likelihood_fixed_A_variable tree length smodel sequenceData = do
  let rtree = setRoot substRoot (makeRooted tree)
      substRoot = modifiable (head $ internalNodes rtree ++ leafNodes rtree)

  let (isequences, column_counts, mapping) = compressAlignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap rtree isequences
      maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      n_nodes = numNodes rtree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cachedConditionalLikelihoods rtree nodeCLVs transitionPs
      likelihood = peelLikelihood nodeCLVs rtree cls f alphabet smap substRoot column_counts

      -- computing the probability of the condition
      (isequences2, column_counts2) = compressAlignmentVarNonvar (getSequences sequenceData) alphabet
      maybeNodeISequences2 = labelToNodeMap rtree isequences2
      maybeNodeSeqsBits2 = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences2
      nodeCLVs2 = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits2
      cls2 = cachedConditionalLikelihoods rtree nodeCLVs2 transitionPs
      likelihood2 = peelLikelihoodVariable nodeCLVs2 rtree cls2 f alphabet smap substRoot column_counts2

      ancestralComponentStates = sampleAncestralSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = (PhyloCTMCPropertiesFixedA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree)) ancestralComponentStates

  return ([likelihood,1/likelihood2], prop)

instance Dist (PhyloCTMC t Int s r) => Dist (VariablePhyloCTMC t s r) where
    type Result (VariablePhyloCTMC t s r) = Result (PhyloCTMC t Int s r)
    dist_name (Variable dist) = "Variable" ++ dist_name dist

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType (Rooted t) ~ Text, HasRoot (Rooted t), HasBranchLengths (Rooted t), RateModel s, IsTree t, SimpleSModel (Rooted t) s) => HasAnnotatedPdf (VariablePhyloCTMC t s EquilibriumReversible) where
    type DistProperties (VariablePhyloCTMC t s EquilibriumReversible) = DistProperties (PhyloCTMC t Int s EquilibriumReversible)
    annotated_densities (Variable (PhyloCTMC tree length smodel scale)) = annotated_subst_likelihood_fixed_A_variable tree length (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => IOSampleable (VariablePhyloCTMC t s EquilibriumReversible) where
    sampleIO (Variable (PhyloCTMC tree rootLength rawSmodel scale)) = do
      let rtree = makeRooted tree
          alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStatesFixed rtree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths t, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => Sampleable (VariablePhyloCTMC t s EquilibriumReversible) where
    sample (Variable dist) = RanDistribution2 dist do_nothing


