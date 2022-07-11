module SModel.EigenExp where

data EigenSystem

foreign import bpcall "SModel:" get_eigensystem :: Matrix Double -> EVector Double -> EigenSystem
foreign import bpcall "SModel:" lExp :: EigenSystem -> EVector Double -> Double -> Matrix Double
