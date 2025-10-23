module Probability.Distribution.PhyloCTMC.FixedA.Reversible where

import Probability.Distribution.PhyloCTMC.FixedA.Properties
import Probability.Distribution.PhyloCTMC.FixedA.Sample
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.FixedA
import Bio.Alignment -- for many things.
import Bio.Alphabet (HasAlphabet(..))
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap

annotatedSubstLikelihoodFixedA tree length smodel sequenceData = do
  let rtree = setRoot substRoot (makeRooted tree)
      substRoot = modifiable (head $ internalNodes rtree ++ leafNodes rtree)

  let (isequences, columnCounts, mapping) = compressAlignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap rtree isequences
      maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cachedConditionalLikelihoods rtree nodeCLVs transitionPs
      likelihood = peelLikelihood nodeCLVs rtree cls f alphabet smap substRoot columnCounts

      ancestralComponentStates = sampleAncestralSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = PhyloCTMCPropertiesFixedA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s EquilibriumReversible) where
    type Result (PhyloCTMC t Int s EquilibriumReversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType (Rooted t) ~ Text, HasRoot (Rooted t), HasBranchLengths (Rooted t), RateModel s, IsTree t, SimpleSModel (Rooted t) s) => HasAnnotatedPdf (PhyloCTMC t Int s EquilibriumReversible) where
    type DistProperties (PhyloCTMC t Int s EquilibriumReversible) = PhyloCTMCPropertiesFixedA
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedA tree length (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => IOSampleable (PhyloCTMC t Int s EquilibriumReversible) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let rtree = makeRooted tree
          alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStatesFixed rtree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => Sampleable (PhyloCTMC t Int s EquilibriumReversible) where
    sample dist = RanDistribution2 dist do_nothing

