module EigenExp where

import Numeric.LinearAlgebra
import Foreign.Maybe    

data EigenSystem

data NoDecompReason = NoDiagReason deriving Show

data MatDecomp = NoDecomp (Maybe NoDecompReason) | RealEigenDecomp EigenSystem

foreign import bpcall "Matrix:" getEigensystemRaw :: Matrix Double -> EVector Double -> CMaybe EigenSystem
foreign import bpcall "Matrix:" lExpRaw :: EigenSystem -> EVector Double -> Double -> CMaybe (Matrix Double)

getEigensystem q pi = fromCMaybe $ getEigensystemRaw q pi
lExp esystem pi factor = fromCMaybe $ lExpRaw esystem pi factor

instance Show MatDecomp where
    show (NoDecomp reason) = "NoDecomp " ++ show reason
    show (RealEigenDecomp _) = "RealEigenDecomp"
