module Probability.Distribution.PhyloCTMC.FixedA.Variable where

import Probability.Distribution.PhyloCTMC.FixedA.Reversible () -- for instances
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

data VariablePhyloCTMC t s = Variable (PhyloCTMC t Int s)

variable = Variable

annotated_subst_likelihood_fixed_A_variable tree length smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes rtree ++ leafNodes rtree)
      rtree = if isReversible smodel
              then setRoot substRoot tree
              else tree

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
      cls = cachedConditionalLikelihoodsNonRev rtree nodeCLVs transitionPs f
      likelihood = peelLikelihoodNonRev nodeCLVs rtree cls f alphabet smap substRoot columnCounts

      -- computing the probability of the condition
      (isequences2, columnCounts2) = compressAlignmentVarNonvar (getSequences sequenceData) alphabet
      maybeNodeISequences2 = labelToNodeMap rtree isequences2
      maybeNodeSeqsBits2 = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences2
      nodeCLVs2 = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits2
      cls2 = cachedConditionalLikelihoodsNonRev rtree nodeCLVs2 transitionPs f
-- How about peelLikelihoodVariableNonRev?             
      likelihood2 = peelLikelihoodVariable nodeCLVs2 rtree cls2 f alphabet smap substRoot columnCounts2

      ancestralComponentStates = sampleAncestralSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = (PhyloCTMCPropertiesFixedA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree)) ancestralComponentStates

  return ([likelihood,1/likelihood2], prop)

instance Dist (PhyloCTMC t Int s) => Dist (VariablePhyloCTMC t s) where
    type Result (VariablePhyloCTMC t s) = Result (PhyloCTMC t Int s)
    dist_name (Variable dist) = "Variable" ++ dist_name dist

-- TODO: make this work on forests!
instance (CheckReversible s, HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (VariablePhyloCTMC t s) where
    type DistProperties (VariablePhyloCTMC t s) = DistProperties (PhyloCTMC t Int s)
    annotated_densities (Variable (PhyloCTMC tree length smodel scale)) = annotated_subst_likelihood_fixed_A_variable tree length (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (VariablePhyloCTMC t s) where
    sampleIO (Variable (PhyloCTMC rtree rootLength rawSmodel scale)) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStatesFixed rtree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (CheckReversible s, HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (VariablePhyloCTMC t s) where
    sample (Variable dist) = RanDistribution2 dist do_nothing


