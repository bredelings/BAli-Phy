module SModel (module SModel,
               module SModel.Nucleotides,
               module SModel.Doublets,
               module SModel.Codons,
               module SModel.ReversibleMarkov,
               module SModel.NonReversibleMarkov,
               module SModel.Parsimony,
               module SModel.Simple,
               module SModel.Rate,
               module SModel.MixtureModel,
               module SModel.MixtureModels,
               module SModel.Empirical,
               module SModel.MarkovModulated,
               module SModel.MutSel,
               module SModel.MultiFrequency,
               module SModel.BranchModel,
               module SModel.RNAEdit,
               module SModel.BranchSiteMixture,
               module SModel.PosSelection,
               module SModel.ASRV,
               module SModel.Property,
               frequenciesFromDict) where

import SModel.Nucleotides
import SModel.Doublets
import SModel.Codons
import SModel.ReversibleMarkov
import SModel.NonReversibleMarkov
import SModel.Parsimony
import SModel.Simple
import SModel.Rate
import SModel.MixtureModel
import SModel.MixtureModels
import SModel.Empirical
import SModel.MarkovModulated
import SModel.MultiFrequency
import SModel.MutSel
import SModel.BranchModel
import SModel.RNAEdit
import SModel.BranchSiteMixture
import SModel.PosSelection
import SModel.ASRV
import SModel.Property

infixl 2 +>
submodel +> model = model submodel


