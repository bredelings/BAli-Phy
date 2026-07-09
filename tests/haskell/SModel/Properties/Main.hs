{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet
import Compiler.Error (error)
import Compiler.Fractional
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Function (($))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe
import Data.OldList ((!!), drop, take)
import Data.Ord
import Data.Text (pack)
import Foreign.Vector (toVector)
import qualified Markov as CoreMarkov
import Probability.Distribution.PhyloCTMC.FixedA.Properties
import Probability.Distribution.PhyloCTMC.Properties
import SModel
import SModel.Property
import System.IO (putStrLn)
import Text.Show (show)

near x y = abs (x - y) < 1.0e-9

-- Retrieve a named property from a property map so the test fails immediately
-- if a required property is absent.
lookupProperty name properties =
    case Map.lookup (pack name) properties of
      Just property -> property
      Nothing       -> error "missing property"

-- Pull the first state value out of a state vector for compact test output.
firstValue values =
    case values of
      x:_ -> x
      []  -> error "missing state value"

componentFirstValues (ComponentStateProperties components) = [firstValue values | StateProperties values <- components]

-- Extract the only component from a property that should have been flattened
-- to one output Markov component.
singleComponentValues property =
    case getComponentStateProperties property of
      [StateProperties values] -> values
      _                        -> error "expected one component"

-- Check rate, dN/dS, Markov-modulated, missing-property, and observable
-- PhyloCTMC property behavior through stable printed results.
main = do
  let base = always (jukes_cantor dna)
      rates = Discrete [(0.5, 0.5), (1.5, 0.5)]
      plusInvModel = plusInv 0.25 (rateMixture base rates)
      rateValues = componentFirstValues $ lookupProperty "rate" $ getProperties (SModelOnTree () plusInvModel)

  putStrLn $ show [near (rateValues!!0) (2.0/3.0), near (rateValues!!1) 2.0, near (rateValues!!2) 0.0]

  let codons = mkCodons dna standard_code
      codonModel = dNdS 2.0 (x3 codons (jukes_cantor dna))
      codonProperties = getProperties (SModelOnTree () codonModel)
      dndsValue = firstValue $ singleComponentValues $ lookupProperty "dNdS" codonProperties
      posSelectionValue = firstValue $ singleComponentValues $ lookupProperty "posSelection" codonProperties

  putStrLn $ show [dndsValue, posSelectionValue]

  let model1 = setConstantStateProperty (pack "dNdS") 0.5 (jukes_cantor dna)
      model2 = setConstantStateProperty (pack "dNdS") 2.0 (jukes_cantor dna)
      between = CoreMarkov.gtr (CoreMarkov.equ 2 1) (toVector [0.5, 0.5])
      modulated = modulatedMarkov [model1, model2] between
      modulatedValues = singleComponentValues $ lookupProperty "dNdS" $ getProperties (SModelOnTree () modulated)

  putStrLn $ show $ take 4 modulatedValues
  putStrLn $ show $ take 4 $ drop 4 modulatedValues

  let scaled1 = setStateProperty (pack "scaled") (\scale -> constantStateProperties 4 (scale * 3.0)) (jukes_cantor dna)
      scaled2 = setStateProperty (pack "scaled") (\scale -> constantStateProperties 4 (scale * 5.0)) (jukes_cantor dna)
      scaledModulated = scaleBy 2.0 $ modulatedMarkov [scaled1, scaled2] between
      scaledValues = singleComponentValues $ lookupProperty "scaled" $ getProperties (SModelOnTree () scaledModulated)

  putStrLn $ show [near (scaledValues!!0) 6.0, near (scaledValues!!4) 10.0]

  let taggedCodonModel = setConstantStateProperty (pack "x") 7.0 (x3 codons (jukes_cantor dna))
      transformedCodonProperties = getProperties (SModelOnTree () (dNdS 2.0 taggedCodonModel))

  putStrLn $ show [Map.member (pack "x") transformedCodonProperties, Map.member (pack "dNdS") transformedCodonProperties]

  let withProperty = setConstantStateProperty (pack "x") 7.0 (jukes_cantor dna)
      withoutProperty = jukes_cantor dna
      partialMixture = Discrete [(withProperty, 0.5), (withoutProperty, 0.5)]
      partialProperties = getProperties (SModelOnTree () partialMixture)

  putStrLn $ show $ Map.member (pack "x") partialProperties

  let branchSite = BranchSiteMixture (always (jukes_cantor dna)) SameEqs Map.empty
      branchSiteRates = componentFirstValues $ lookupProperty "rate" $
        getProperties (SModelOnTree () (rateMixture (always branchSite) rates))

  putStrLn $ show [near (branchSiteRates!!0) 0.5, near (branchSiteRates!!1) 1.5]

  let fixedProperties = PhyloCTMCPropertiesFixedA 0 IntMap.empty IntMap.empty 1 dna 4 1
                          IntMap.empty
                          (Map.singleton (pack "x") (singletonComponentProperty (StateProperties [7.0])))

  putStrLn $ show $ Map.member (pack "x") $ prop_smodel_properties fixedProperties
