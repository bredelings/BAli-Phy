module EigenExp where

import Compiler.FFI.Import (CInput)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Foreign.Maybe    

data EigenSystem

instance CInput EigenSystem

data NoDecompReason = NoDiagReason deriving Show

data MatDecomp = NoDecomp (Maybe NoDecompReason) | RealEigenDecomp EigenSystem

foreign import trcall "Matrix:getEigensystemRaw" getEigensystemNative :: Matrix Double -> Vector Double -> CMaybe EigenSystem
foreign import trcall "Matrix:lExpRaw" lExpNative :: EigenSystem -> Vector Double -> Double -> Maybe (Matrix Double)

getEigensystem q pi = fromCMaybe $
    getEigensystemNative q pi

-- Wrap a successful eigensystem exponential with the frequency dimension.
lExp esystem pi factor =
    case lExpNative esystem pi factor of
        Nothing -> Nothing
        Just matrix -> Just (overrideMatrixDims dimension dimension matrix)
  where dimension = vectorSize pi

instance Show MatDecomp where
    show (NoDecomp reason) = "NoDecomp " ++ show reason
    show (RealEigenDecomp _) = "RealEigenDecomp"
