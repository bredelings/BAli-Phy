{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet
import Compiler.Error (error)
import Compiler.Floating (exp, sqrt)
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat (isNaN)
import Data.Bool
import Data.Eq
import Data.Foldable (all, sum)
import Data.Function (($))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Maybe
import Data.OldList ((!!), and, drop, length, replicate, reverse, take, zipWith)
import Data.Ord
import Data.Text (pack)
import qualified EigenExp
import Numeric.LinearAlgebra (Matrix, cols, flatten, fromList, fromLists, rows, toList)
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

lookupCondition name conditions =
    case Map.lookup (pack name) conditions of
      Just value -> value
      Nothing    -> error "missing condition"

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

-- Compute the mixture-weighted rate for one category of a branch model.
weightedBranchCategoryRate category (Discrete components) =
    sum [probability * rate (models!!category)
        | (BranchModel _ models _ _, probability) <- components]

-- Check model properties and native substitution-model numeric arrays through stable printed
-- results, including propagation of a defective rate without entering the matrix eigensolver.
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

  -- Exercise the component-condition contract where existing property tests cannot detect
  -- loss during mixture reduction, modulation, scaling, or a Q-reconstructing modifier.
  let neutralCodonModel = dNdS 0.5 (x3 codons (jukes_cantor dna))
      mixedCodonModel = Discrete [(neutralCodonModel, 0.5), (codonModel, 0.5)]
      partialConditionModel = Discrete [(codonModel, 0.5), (x3 codons (jukes_cantor dna), 0.5)]
  putStrLn $ show
    [ lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () codonModel)
    , lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () neutralCodonModel)
    , lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () mixedCodonModel)
    , Map.member (pack "positiveSelectionInModel") $ getConditions (SModelOnTree () partialConditionModel)
    , lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () (scaleBy 3 codonModel))
    ]

  -- Ensure an identity-state modifier preserves arbitrary construction annotations,
  -- and later omega and rate mixtures transport them to every final component.
  let zeroFitness = replicate (alphabetSize codons) 0
      taggedOmegaModel omega =
        mut_sel zeroFitness $
        setComponentCondition (pack "markerCondition") True $
        setConstantStateProperty (pack "marker") 7 $
        dNdS omega (x3 codons (jukes_cantor dna))
      omegaMixture = Discrete [(taggedOmegaModel 0.5, 0.5), (taggedOmegaModel 2.0, 0.5)]
      omegaRateMixture = rateMixture omegaMixture rates
      propagatedProperties = getProperties (SModelOnTree () omegaRateMixture)
      propagatedConditions = getConditions (SModelOnTree () omegaRateMixture)
  putStrLn $ show
    ( ( componentFirstValues $ lookupProperty "dNdS" propagatedProperties
      , componentFirstValues $ lookupProperty "posSelection" propagatedProperties
      , componentFirstValues $ lookupProperty "marker" propagatedProperties
      )
    , ( lookupCondition "positiveSelectionInModel" propagatedConditions
      , lookupCondition "markerCondition" propagatedConditions
      )
    )

  let triangle = symmetricMatrixFromLowerTriangle 3 [1, 2, 3]
      nonReversible = nonRev dna (replicate 12 1)
      weighted = weightedFrequencyMatrixFromVectors (fromList [0.25, 0.75])
                   [fromList [0.5, 0.5], fromList [0.25, 0.75]]
      selected = mut_sel (replicate (alphabetSize codons) 0) codonModel

  putStrLn $ show (rows triangle, cols triangle, toList (flatten triangle))
  putStrLn $ show $ Map.size $ wag_frequencies aa
  putStrLn $ show (rows (CoreMarkov.getQ nonReversible), cols (CoreMarkov.getQ nonReversible))
  putStrLn $ show (rows weighted, cols weighted, toList (flatten weighted))
  putStrLn $ show $ length $ toList $ CoreMarkov.getStartFreqs selected

  let model1 = setConstantStateProperty (pack "dNdS") 0.5 (jukes_cantor dna)
      model2 = setConstantStateProperty (pack "dNdS") 2.0 (jukes_cantor dna)
      between = CoreMarkov.gtr (CoreMarkov.equ 2 1) [0.5, 0.5]
      modulated = modulatedMarkov AlwaysSwitch [model1, model2] between
      modulatedValues = singleComponentValues $ lookupProperty "dNdS" $ getProperties (SModelOnTree () modulated)

  putStrLn $ show $ toList $ CoreMarkov.getStartFreqs between
  putStrLn $ show [ CoreMarkov.checkStationary (CoreMarkov.getQ between) (CoreMarkov.getStartFreqs between)
                  , CoreMarkov.checkReversible (CoreMarkov.getQ between) (CoreMarkov.getEqFreqs between)
                  ]
  putStrLn $ show $ toList $ CoreMarkov.equilibriumLimit (fromList [1, 0]) (CoreMarkov.getQ between)
  putStrLn $ show (rows (CoreMarkov.qExp between), cols (CoreMarkov.qExp between))
  putStrLn $ show $ all isNaN $ toList $ flatten $ CoreMarkov.qExp $ scaleBy (0.0 / 0.0) between

  -- Protect large-time reversible exponentiation from a slightly positive stationary eigenvalue;
  -- ordinary substitution-model tests do not reliably control the eigensolver's rounding direction.
  let almostRate = fromLists [[-1 + 1.0e-12, 1], [1, -1 + 1.0e-12]]
      equalFrequencies = fromList [0.5, 0.5]
      stableLargeExponential =
        case EigenExp.getEigensystem almostRate equalFrequencies of
          Nothing -> False
          Just eigensystem ->
            case EigenExp.lExp eigensystem equalFrequencies 1.0e20 of
              Nothing -> False
              Just transition -> all (\x -> near x 0.5) $ toList $ flatten transition
  putStrLn $ show stableLargeExponential

  putStrLn $ show $ take 4 modulatedValues
  putStrLn $ show $ take 4 $ drop 4 modulatedValues

  let modulatedSelection = modulatedMarkov AlwaysSwitch [neutralCodonModel, codonModel] between
  putStrLn $ show $ lookupCondition "positiveSelectionInModel" $
    getConditions (SModelOnTree () modulatedSelection)

  let scaled1 = setStateProperty (pack "scaled") (\scale -> constantStateProperties 4 (scale * 3.0)) (jukes_cantor dna)
      scaled2 = setStateProperty (pack "scaled") (\scale -> constantStateProperties 4 (scale * 5.0)) (jukes_cantor dna)
      scaledModulated = scaleBy 2.0 $ modulatedMarkov AlwaysSwitch [scaled1, scaled2] between
      scaledValues = singleComponentValues $ lookupProperty "scaled" $ getProperties (SModelOnTree () scaledModulated)

  putStrLn $ show [near (scaledValues!!0) 6.0, near (scaledValues!!4) 10.0]

  let taggedCodonModel = setConstantStateProperty (pack "x") 7.0 (x3 codons (jukes_cantor dna))
      transformedCodonProperties = getProperties (SModelOnTree () (dNdS 2.0 taggedCodonModel))

  putStrLn $ show
    [ Map.member (pack "x") transformedCodonProperties
    , Map.member (pack "dNdS") transformedCodonProperties
    ]
  putStrLn $ show $ Map.member (pack "positiveSelectionInModel") $ getConditions (SModelOnTree () selected)

  let withProperty = setConstantStateProperty (pack "x") 7.0 (jukes_cantor dna)
      withoutProperty = jukes_cantor dna
      partialMixture = Discrete [(withProperty, 0.5), (withoutProperty, 0.5)]
      partialProperties = getProperties (SModelOnTree () partialMixture)

  putStrLn $ show $ Map.member (pack "x") partialProperties

  let branchSite = BranchSiteMixture (always (jukes_cantor dna)) SameEqs emptyComponentAnnotations
      branchSiteRates = componentFirstValues $ lookupProperty "rate" $
        getProperties (SModelOnTree () (rateMixture (always branchSite) rates))

  putStrLn $ show [near (branchSiteRates!!0) 0.5, near (branchSiteRates!!1) 1.5]

  let fixedProperties = PhyloCTMCPropertiesFixedA 0 IntMap.empty IntMap.empty 1 dna 4 1
                          IntMap.empty
                          (Map.singleton (pack "x") (singletonComponentProperty (StateProperties [7.0])))
                          (Map.singleton positiveSelectionInModelConditionName True)

  putStrLn $ show $ Map.member (pack "x") $ prop_smodel_properties fixedProperties
  putStrLn $ show $ prop_smodel_conditions fixedProperties ==
    Map.singleton positiveSelectionInModelConditionName True

  let branchCategories = IntMap.fromList [(0, 0), (1, 1)]
      modelForOmega omega =
        setComponentCondition positiveSelectionInModelConditionName (omega > 1) $
        setConstantStateProperty (pack "marker") (omega + 10) $
        setConstantStateProperty posSelectionPropertyName (if omega > 1 then 1 else 0) $
        setConstantStateProperty dNdSPropertyName omega $
        scaleBy omega (jukes_cantor dna)
      branchModel = branchSiteATest [0.6, 0.4] [0.25] 0.2 3.0 1 branchCategories modelForOmega
      nullBranchModel = branchSiteATest [0.6, 0.4] [0.25] 0.2 3.0 0 branchCategories modelForOmega
      Discrete branchComponents = branchModel
      branchWeights = [probability | (_, probability) <- branchComponents]
      scaledBranchModel = scaleBy 2.0 branchModel
      branchProperties = getProperties (SModelOnTree () branchModel)
      nullBranchProperties = getProperties (SModelOnTree () nullBranchModel)

  putStrLn $ show $ zipWith near branchWeights [0.48, 0.32, 0.12, 0.08]
  putStrLn $ show [ near (weightedBranchCategoryRate 0 branchModel) 1.0
                    , near (weightedBranchCategoryRate 1 branchModel) 1.0
                    , near (rate branchModel) 1.0
                    , near (weightedBranchCategoryRate 0 scaledBranchModel) 2.0
                    , near (weightedBranchCategoryRate 1 scaledBranchModel) 2.0
                    ]
  putStrLn $ show $ componentFirstValues $ lookupProperty "background-dNdS" branchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "foreground-dNdS" branchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "foreground-posSelection" branchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "background-marker" branchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "foreground-marker" branchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "foreground-dNdS" nullBranchProperties
  putStrLn $ show $ componentFirstValues $ lookupProperty "foreground-posSelection" nullBranchProperties
  putStrLn $ show
    [ lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () branchModel)
    , lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () nullBranchModel)
    ]

  let bustedAlternative = busted (always 0.5) 0.2 3.0 1 modelForOmega
      bustedNull = busted (always 0.5) 0.2 3.0 0 modelForOmega
  putStrLn $ show
    [ lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () bustedAlternative)
    , lookupCondition "positiveSelectionInModel" $ getConditions (SModelOnTree () bustedNull)
    ]

  -- Check equilibrium and root-frequency behavior after excluding stop codons.
  -- Successful MCMC and property logging can miss a model incorrectly marked stationary.
  -- Retain while x3 supports stationary non-reversible and non-equilibrium inputs.
  let cyclicRates = fromLists [[-7,5,1,1],[1,-7,5,1],[1,1,-7,5],[5,1,1,-7]]
      cyclicNuc = eqMarkov dna (simpleSMap dna) (getLettersVector dna) cyclicRates
      rootNuc = markov dna (simpleSMap dna) (getLettersVector dna) cyclicRates (fromList [0.1,0.2,0.3,0.4])
      cyclicCodon = x3 codons cyclicNuc
      rootCodon = x3 codons rootNuc
      productRoot = f3x4_frequencies_builtin codons
                      (CoreMarkov.getStartFreqs rootNuc) (CoreMarkov.getStartFreqs rootNuc)
                      (CoreMarkov.getStartFreqs rootNuc)
  putStrLn $ show ("x3 frequencies",
    [ getReversibility cyclicCodon == EqNonRev
    , CoreMarkov.checkStationary (CoreMarkov.getQ cyclicCodon) (CoreMarkov.getStartFreqs cyclicCodon)
    , not $ CoreMarkov.checkReversible (CoreMarkov.getQ cyclicCodon) (CoreMarkov.getStartFreqs cyclicCodon)
    , getReversibility rootCodon == NonEq
    , and $ zipWith near (toList productRoot) (toList $ CoreMarkov.getStartFreqs rootCodon)
    ])

  -- Check equilibrium and root-frequency behavior when omega changes the rates.
  -- Existing omega-property tests check annotations, not frequencies.
  -- Retain while dNdS supports stationary non-reversible and non-equilibrium inputs.
  let selectedCyclic = dNdS 2 cyclicCodon
      selectedRoot = dNdS 2 rootCodon
  putStrLn $ show ("dNdS frequencies",
    [ getReversibility selectedCyclic == EqNonRev
    , CoreMarkov.checkStationary (CoreMarkov.getQ selectedCyclic) (CoreMarkov.getStartFreqs selectedCyclic)
    , not $ CoreMarkov.checkReversible (CoreMarkov.getQ selectedCyclic) (CoreMarkov.getStartFreqs selectedCyclic)
    , getReversibility selectedRoot == NonEq
    , and $ zipWith near (toList productRoot) (toList $ CoreMarkov.getStartFreqs selectedRoot)
    ])

  -- Check equilibrium and root-frequency behavior with unequal fitnesses.
  -- Existing zero-fitness and annotation tests miss incorrect frequency handling.
  -- Retain while MutSel supports stationary non-reversible and non-equilibrium inputs.
  let fitnesses = [0,1,-1,2]
      fitCyclic = mut_sel fitnesses cyclicNuc
      fitRoot = mut_sel fitnesses rootNuc
  putStrLn $ show ("MutSel frequencies",
    [ getReversibility fitCyclic == EqNonRev
    , CoreMarkov.checkStationary (CoreMarkov.getQ fitCyclic) (CoreMarkov.getStartFreqs fitCyclic)
    , getReversibility fitRoot == NonEq
    , and $ zipWith near (toList $ CoreMarkov.getStartFreqs rootNuc)
                        (toList $ CoreMarkov.getStartFreqs fitRoot)
    ])

  -- Check equilibrium and root-frequency behavior with multinucleotide mutations.
  -- Reversible event-rate tests miss incorrect stationarity labels and NonEq root frequencies.
  -- Retain while MNM supports stationary non-reversible and non-equilibrium inputs.
  let mnmCyclic = mnm codons 0.2 0.03 cyclicNuc
      mnmRoot = mnm codons 0.2 0.03 rootNuc
  putStrLn $ show ("MNM frequencies",
    [ getReversibility mnmCyclic == EqNonRev
    , CoreMarkov.checkStationary (CoreMarkov.getQ mnmCyclic) (CoreMarkov.getStartFreqs mnmCyclic)
    , getReversibility mnmRoot == NonEq
    , and $ zipWith near (toList productRoot) (toList $ CoreMarkov.getStartFreqs mnmRoot)
    ])

  -- Frequency attachment must agree with its endpoint formulas and retain finite zero-frequency
  -- F81 transitions. Positive-frequency model tests miss removable singularities in this kernel.
  -- Retain while F and gwF share the native frequency-factor calculation.
  let ps = [0.25,0.75]
      indices = [0,1]
      factor f = toList $ flatten $ CoreMarkov.plus_gwf_matrix (fromList ps) f
      zeroFactor f = toList $ flatten $ CoreMarkov.plus_gwf_matrix (fromList [0,1]) f
      zeroPs = [0,0.25,0.25,0.5]
      zeroModel = f81 zeroPs dna
      decay = exp (-1/0.625)
  putStrLn $ show ("frequency factors",
    [ and $ zipWith near (factor 0) [0,4,4/3,0]
    , and $ zipWith near (factor 1) [0,0.75,0.25,0]
    , and $ zipWith near (factor 0.5)
        [if i == j then 0 else sqrt ((ps!!j)/(ps!!i)) | i <- indices, j <- indices]
    , and $ zipWith near (zeroFactor 1) [0,1,0,0]
    , and $ zipWith near (zeroFactor 2) [0,0,0,0]
    , and $ zipWith near (toList $ flatten $ CoreMarkov.getQ zeroModel)
        [(if i == j then zeroPs!!j-1 else zeroPs!!j)/0.625
        | i <- [0,1,2,3], j <- [0,1,2,3]]
    , and $ zipWith near (toList $ flatten $ CoreMarkov.qExp zeroModel)
        [(if i == j then decay else 0) + (1-decay)*(zeroPs!!j)
        | i <- [0,1,2,3], j <- [0,1,2,3]]
    ])

  -- Ordinary models do not expose hidden-state naming errors. Check expanded ordering and
  -- annotation-preserving root replacement here alongside the existing model-property checks.
  let hidden = tuffleySteel98 0.2 0.3 model1
      namedRoots = Map.fromList [(name, 0.125) | name <- stateNames hidden]
      rootedHidden = nonEq' namedRoots (setComponentCondition (pack "flag") True hidden)
      nested = modulatedMarkov AlwaysSwitch [hidden, hidden] between
  putStrLn $ show ("state names",
    [ stateNames (jukes_cantor dna) == getLetters dna
    , stateNames hidden == ["OFF:A","OFF:C","OFF:G","OFF:T","ON:A","ON:C","ON:G","ON:T"]
    , length (stateNames nested) == 16
    , take 2 (stateNames nested) == ["1:OFF:A","1:OFF:C"]
    , stateNames (scaleBy 2 hidden) == stateNames hidden
    , stateNames rootedHidden == stateNames hidden
    , Map.size (labelledStartFrequencies rootedHidden) == 8
    , and $ zipWith near (toList $ CoreMarkov.getStartFreqs rootedHidden) (replicate 8 0.125)
    , Map.member (pack "dNdS") (getStatePropertyFunctions rootedHidden)
    , lookupCondition "flag" (getComponentConditions rootedHidden)
    , stateNames selected == getLetters codons
    ])

  -- Named fitnesses must follow matrix order, including hidden states; list-only tests miss
  -- alphabet-based truncation and map-order mistakes. Retain while MutSel accepts named maps.
  let namedHidden = case hidden of
        Markov a smap names process modelRate annotations ->
          Markov a smap (V.fromList $ reverse $ V.toList names) process modelRate annotations
      hiddenFitnesses = [0,1,-1,2,3,-2,0.5,1.5]
      fitnessMap = Map.fromList $ zipWith (\name value -> (name,value)) (stateNames namedHidden) hiddenFitnesses
      namedSelected = mut_sel' fitnessMap namedHidden
      orderedSelected = mut_sel hiddenFitnesses namedHidden
  putStrLn $ show ("named MutSel fitnesses",
    [ stateNames namedSelected == stateNames namedHidden
    , and $ zipWith near (toList $ flatten $ CoreMarkov.getQ namedSelected)
                        (toList $ flatten $ CoreMarkov.getQ orderedSelected)
    , and $ zipWith near (toList $ CoreMarkov.getStartFreqs namedSelected)
                        (toList $ CoreMarkov.getStartFreqs orderedSelected)
    ])

  -- Matrix reports must preserve pair orientation with multi-character names. Frequency reports
  -- do not cover pair labels; retain while these helpers generate keyed matrix reports.
  let reportNames = V.fromList ["ON:A", "OFF:A", "C"]
      reportMatrix :: Matrix Double
      reportMatrix = fromLists [[0,1,2], [3,0,4], [5,6,0]]
  putStrLn $ show ("matrix report labels",
    [ labelledUpperTriangle reportNames ">" reportMatrix ==
        Map.fromList [("ON:A>OFF:A",1), ("ON:A>C",2), ("OFF:A>C",4)]
    , labelledOffDiagonal reportNames "|" reportMatrix ==
        Map.fromList [("ON:A|OFF:A",1), ("ON:A|C",2), ("OFF:A|ON:A",3),
                      ("OFF:A|C",4), ("C|ON:A",5), ("C|OFF:A",6)]
    ])

  -- Check both switching constructions together: property propagation above cannot detect wrong
  -- equilibrium claims or acceptance factors. Retain while the methods share a native builder.
  let componentA = unscaled_f81 [0.4,0.3,0.2,0.1] dna
      componentB = unscaled_f81 [0.1,0.2,0.3,0.4] dna
      switching = CoreMarkov.gtr (CoreMarkov.equ 2 1) [0.25,0.75]
      direct = modulatedMarkov AlwaysSwitch [componentA,componentB] switching
      metropolis = modulatedMarkov MetropolisSwitching [componentA,componentB] switching
      qValues model = toList $ flatten $ CoreMarkov.getQ model
      sameMatrix a b = and $ zipWith near (qValues a) (qValues b)
      stationary model = CoreMarkov.checkStationary (CoreMarkov.getQ model) (CoreMarkov.getStartFreqs model)
      classify method equal r s = getReversibility $ modulatedMarkov method
          [setReversibility r componentA, setReversibility r (if equal then componentA else componentB)]
          (setReversibility s switching)
      classifications = [EqRev,EqNonRev,NonEq]
  putStrLn $ show ("modulation classifications",
    [ and [classify AlwaysSwitch True r s == min r s | r <- classifications, s <- classifications]
    , and [classify AlwaysSwitch False r s == NonEq | r <- classifications, s <- classifications]
    , and [classify MetropolisSwitching True r s == min r s | r <- classifications, s <- classifications]
    , and [classify MetropolisSwitching False r s == (if s == EqRev then r else NonEq)
          | r <- classifications, s <- classifications]
    ])
  putStrLn $ show ("modulation rates",
    [ not (stationary direct), stationary metropolis
    , CoreMarkov.checkReversible (CoreMarkov.getQ metropolis) (CoreMarkov.getStartFreqs metropolis)
    , near (qValues direct !! 4) 0.75
    , near (qValues metropolis !! 4) 0.1875
    , near (qValues metropolis !! 32) 0.25
    , and $ zipWith near (toList $ CoreMarkov.getStartFreqs metropolis)
                        [0.1,0.075,0.05,0.025,0.075,0.15,0.225,0.3]
    , CoreMarkov.checkStationary (CoreMarkov.getQ direct) (CoreMarkov.getEqFreqs direct)
    ])

  -- NonEq roots must not become acceptance targets. A directed switching cycle also checks the
  -- actual nonreversible case, rather than only conservative classification of reversible inputs.
  let replaceStart model start = markov dna (simpleSMap dna) (getLettersVector dna) (CoreMarkov.getQ model) (fromList start)
      changedRoot = replaceStart componentA [0.1,0.2,0.3,0.4]
      nonEqModulated = modulatedMarkov MetropolisSwitching [changedRoot,componentB] switching
      cycleQ = fromLists [[-1,1,0],[0,-1,1],[1,0,-1]]
      cycleSwitch = setReversibility EqNonRev $ CoreMarkov.markov cycleQ (fromList [1/3,1/3,1/3])
      cycleEqual = modulatedMarkov MetropolisSwitching [componentA,componentA,componentA] cycleSwitch
      cycleDifferent = modulatedMarkov MetropolisSwitching [componentA,componentB,componentA] cycleSwitch
      zeroA = unscaled_f81 [0,0.5,0.5,0] dna
      zeroB = unscaled_f81 [0.5,0,0.5,0] dna
      zeroMetropolis = modulatedMarkov MetropolisSwitching [zeroA,zeroB] switching
  putStrLn $ show ("modulation roots and zeros",
    [ sameMatrix nonEqModulated metropolis
    , getReversibility nonEqModulated == NonEq
    , and $ zipWith near (toList $ CoreMarkov.getStartFreqs nonEqModulated)
                        [0.025,0.05,0.075,0.1,0.075,0.15,0.225,0.3]
    , stationary cycleEqual, not (stationary cycleDifferent)
    , sameMatrix (modulatedMarkov AlwaysSwitch [zeroA,zeroA] switching)
                 (modulatedMarkov MetropolisSwitching [zeroA,zeroA] switching)
    , stationary zeroMetropolis
    , near (qValues zeroMetropolis !! 4) 0.75
    , near (qValues zeroMetropolis !! 13) 0
    , near (qValues zeroMetropolis !! 31) 0.75
    ])

  -- Protect Wang07's nonswitching fraction and relative rates: the general modulation checks
  -- above do not exercise this mixture. Retain while Wang07 combines ON/OFF and class switching.
  let baseRates = Discrete [(scaleBy 0.5 $ scaleTo 1 componentA, 0.5),
                            (scaleBy 1.5 $ scaleTo 1 componentA, 0.5)]
      wang p = wssr07 AlwaysSwitch 1 3 2 p baseRates
      Discrete [(noCov,_),(cov,_)] = wang 0.4
      noCovQ = qValues noCov
      noCovStart = toList $ CoreMarkov.getStartFreqs noCov
  putStrLn $ show ("Wang07 mixture",
    [ near (rate $ wang 0) 1, near (rate $ wang 1) 0.25
    , near (rate $ wang 0.4) 0.7
    , all (near 0) (take 8 noCovStart)
    , near (noCovQ !! (8*16+12)) 0
    , near (noCovQ !! (8*16)) 0
    , near (noCovQ !! (8*16+9)) (0.5 * (qValues (scaleTo 1 componentA) !! 1))
    , sameMatrix cov (wssr07Ssrv AlwaysSwitch 1 3 2 baseRates)
    , stateNames noCov == stateNames cov
    ])
