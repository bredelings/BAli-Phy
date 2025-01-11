module EigenExp where

import Data.Matrix
import Foreign.Maybe    

data EigenSystem

data NoDecompReason = NoDiagReason

data MatDecomp = NoDecomp (Maybe NoDecompReason) | RealEigenDecomp EigenSystem

foreign import bpcall "Matrix:" getEigensystemRaw :: Matrix Double -> EVector Double -> CMaybe EigenSystem
foreign import bpcall "Matrix:" lExpRaw :: EigenSystem -> EVector Double -> Double -> CMaybe (Matrix Double)

getEigensystem q pi = fromCMaybe $ getEigensystemRaw q pi
lExp esystem pi factor = fromCMaybe $ lExpRaw esystem pi factor

instance Show NoDecompReason where
    show NoDiagReason = "NoDiagReason"

instance Show MatDecomp where
    show (NoDecomp reason) = "NoDecomp " ++ show reason
    show (RealEigenDecomp _) = "RealEigenDecomp"
