module EigenExp where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Foreign.Maybe    

data EigenSystem

data NoDecompReason = NoDiagReason deriving Show

data MatDecomp = NoDecomp (Maybe NoDecompReason) | RealEigenDecomp EigenSystem

foreign import bpcall "Matrix:getEigensystemRaw" getEigensystemNative :: NativeMatrix Double -> NativeVector Double -> CMaybe EigenSystem
foreign import bpcall "Matrix:lExpRaw" lExpNative :: EigenSystem -> NativeVector Double -> Double -> CMaybe (NativeMatrix Double)

getEigensystem q pi = fromCMaybe $
    getEigensystemNative (nativeMatrix q) (nativeVector pi)

-- Wrap a successful eigensystem exponential with the frequency dimension.
lExp esystem pi factor =
    case fromCMaybe $ lExpNative esystem (nativeVector pi) factor of
        Nothing -> Nothing
        Just payload -> Just (matrixFromNative dimension dimension payload)
  where dimension = vectorSize pi

instance Show MatDecomp where
    show (NoDecomp reason) = "NoDecomp " ++ show reason
    show (RealEigenDecomp _) = "RealEigenDecomp"
