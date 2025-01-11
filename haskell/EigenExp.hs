module EigenExp where

import Data.Matrix

data EigenSystem

foreign import bpcall "Matrix:" get_eigensystem :: Matrix Double -> EVector Double -> EigenSystem
foreign import bpcall "Matrix:" lExp :: EigenSystem -> EVector Double -> Double -> Matrix Double
