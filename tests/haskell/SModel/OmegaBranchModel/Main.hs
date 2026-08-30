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
import Data.OldList ((!!), and, zipWith)
import Data.Ord
import Data.Text (pack)
import Numeric.LinearAlgebra (flatten, toList)
import qualified Markov as CoreMarkov
import SModel
import System.IO (putStrLn)
import Text.Show (show)

near x y = abs (x - y) < 1.0e-9

-- Retrieve a named property so missing category prefixes fail the test directly.
lookupProperty name properties =
    case Map.lookup (pack name) properties of
      Just property -> property
      Nothing       -> error "missing property"

-- Pull the only value from a property attached uniformly to a model's states.
propertyValue name properties =
    case getComponentStateProperties (lookupProperty name properties) of
      [StateProperties (value:_)] -> value
      _                           -> error "expected one nonempty property component"

-- Compare two rate matrices while allowing only insignificant floating-point differences.
sameMatrix model1 model2 =
    and $ zipWith near (toList $ flatten $ CoreMarkov.getQ model1)
                       (toList $ flatten $ CoreMarkov.getQ model2)

-- Check category counting, category selection, independent normalization, and prefixed properties.
main = do
  let categories = IntMap.fromList [(0, 0), (1, 2), (2, 1)]
      omegas = [0.25, 1.0, 3.0]
      modelForOmega omega =
        setConstantStateProperty (pack "marker") (omega + 10) $
        setConstantStateProperty dNdSPropertyName omega $
        scaleBy omega (jukes_cantor dna)
      model@(BranchModel actualCategories models modelRate _) =
        omegaBranchModel categories omegas modelForOmega
      expectedModels = [scaleTo 1 (modelForOmega omega) | omega <- omegas]
      properties = getProperties (SModelOnTree () model)

  putStrLn $ show [numberBranchCategories categories == 3,
                   IntMap.toList actualCategories == IntMap.toList categories,
                   near modelRate 1.0]
  putStrLn $ show $ zipWith sameMatrix models expectedModels
  putStrLn $ show [propertyValue "branch0-dNdS" properties,
                   propertyValue "branch1-dNdS" properties,
                   propertyValue "branch2-dNdS" properties]
  putStrLn $ show [propertyValue "branch0-marker" properties,
                   propertyValue "branch1-marker" properties,
                   propertyValue "branch2-marker" properties]

  let codons = mkCodons dna standard_code
      omegaDependentModel omega =
        setConstantStateProperty dNdSPropertyName omega $
        dNdS omega (x3 codons (jukes_cantor dna))
      twoCategories = IntMap.fromList [(0, 0), (1, 1)]
      nullModel@(BranchModel _ nullModels _ _) =
        twoOmegaBranchModel twoCategories 0.25 3.0 0 omegaDependentModel
      alternativeModel@(BranchModel _ alternativeModels _ _) =
        twoOmegaBranchModel twoCategories 0.25 3.0 1 omegaDependentModel
      nullProperties = getProperties (SModelOnTree () nullModel)
      alternativeProperties = getProperties (SModelOnTree () alternativeModel)

  putStrLn $ show [branchTestForegroundOmega 0.25 3.0 0,
                   branchTestForegroundOmega 0.25 3.0 1]
  putStrLn $ show [propertyValue "branch0-dNdS" nullProperties,
                   propertyValue "branch1-dNdS" nullProperties]
  putStrLn $ show [propertyValue "branch0-dNdS" alternativeProperties,
                   propertyValue "branch1-dNdS" alternativeProperties]
  putStrLn $ show [sameMatrix (nullModels!!0) (nullModels!!1),
                   sameMatrix (alternativeModels!!0) (alternativeModels!!1)]
