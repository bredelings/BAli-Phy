module Probability.Distribution.PhyloCTMC.FixedA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.FixedA.Properties,
     module Probability.Distribution.PhyloCTMC.FixedA.Reversible,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC,
     VariablePhyloCTMC(..), variable
    )

where

import Probability.Distribution.PhyloCTMC.FixedA.Reversible
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

{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotatedSubstLikelihoodFixedANonRev tree length smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap tree isequences
      maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      n_nodes = numNodes tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cachedConditionalLikelihoodsNonRev tree nodeCLVs transitionPs f
      likelihood = peelLikelihoodNonRev nodeCLVs tree cls f alphabet smap substRoot column_counts

      ancestralComponentStates = sample_ancestral_sequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = PhyloCTMCPropertiesFixedA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s EquilibriumNonReversible) where
    type Result (PhyloCTMC t Int s EquilibriumNonReversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (PhyloCTMC t Int s EquilibriumNonReversible) where
    type DistProperties (PhyloCTMC t Int s EquilibriumNonReversible) = PhyloCTMCPropertiesFixedA
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedANonRev tree length (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t Int s EquilibriumNonReversible) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStatesFixed tree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t Int s EquilibriumNonReversible) where
    sample dist = RanDistribution2 dist do_nothing


--------------

instance Dist (PhyloCTMC t Int s NonEquilibrium) where
    type Result (PhyloCTMC t Int s NonEquilibrium) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (PhyloCTMC t Int s NonEquilibrium) where
    type DistProperties (PhyloCTMC t Int s NonEquilibrium) = PhyloCTMCPropertiesFixedA
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedANonRev tree length (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t Int s NonEquilibrium) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStatesFixed tree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t Int s NonEquilibrium) where
    sample dist = RanDistribution2 dist do_nothing


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

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

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
      cls = cached_conditional_likelihoods rtree nodeCLVs transitionPs
      likelihood = peel_likelihood nodeCLVs rtree cls f alphabet smap substRoot column_counts

      -- computing the probability of the condition
      (isequences2, column_counts2) = compress_alignment_var_nonvar (getSequences sequenceData) alphabet
      maybeNodeISequences2 = labelToNodeMap rtree isequences2
      maybeNodeSeqsBits2 = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences2
      nodeCLVs2 = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits2
      cls2 = cached_conditional_likelihoods rtree nodeCLVs2 transitionPs
      likelihood2 = peel_likelihood_variable nodeCLVs2 rtree cls2 f alphabet smap substRoot column_counts2

      ancestralComponentStates = sample_ancestral_sequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

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


