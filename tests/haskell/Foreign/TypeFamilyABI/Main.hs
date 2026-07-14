{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Compiler.Base (String)
import Compiler.IO (IO)
import System.IO (print, putStrLn)
import Foreign.String (CPPString, pack_cpp_string)

import ImportedABI

type family ClosedABI a where
    ClosedABI Int = Int -> Int -> Int

foreign import ecall "Prelude:div_int"
    closedDiv :: ClosedABI Int

type family OpenABI a
type instance OpenABI Int = Int -> Int -> Int

foreign import ecall "Prelude:div_int"
    openDiv :: OpenABI Int

class HasABI a where
    type AssociatedABI a

instance HasABI Int where
    type AssociatedABI Int = Int -> Int -> Int

foreign import ecall "Prelude:div_int"
    associatedDiv :: AssociatedABI Int

type family InnerABI a where
    InnerABI Int = Int -> Int -> Int

type family OuterABI a where
    OuterABI Int = InnerABI Int

type SynonymABI = OuterABI Int

foreign import ecall "Prelude:div_int"
    nestedDiv :: SynonymABI

foreign import ecall "Prelude:div_int"
    importedDiv :: ImportedABI Int

-- trcall's closed RawImport family is itself family-headed.  The effectful
-- binding also verifies that its translated raw type contributes exactly one
-- world argument.
foreign import trcall "Vector:cppSubString"
    translatedSubstring :: String -> Int -> Int -> String

foreign import trcall "Environment:getEnvRaw"
    translatedGetEnv :: String -> IO String

type family EffectfulABI a where
    EffectfulABI Int = CPPString -> IO CPPString

foreign import bpcall "Environment:getEnvRaw"
    familyGetEnv :: EffectfulABI Int

-- An unknown representation within an already exposed argument slot does not
-- prevent milestone A from determining the operation arity.
type family UnknownSlot a

foreign import ecall "Prelude:div_int"
    unknownSlotDiv :: UnknownSlot Int -> Int -> Int

main = do
    print (closedDiv 20 4
           + openDiv 20 4
           + associatedDiv 20 4
           + nestedDiv 20 4
           + importedDiv 20 4)
    putStrLn (translatedSubstring "abcd" 1 2)
    _ <- familyGetEnv (pack_cpp_string "PATH")
    _ <- translatedGetEnv "PATH"
    print (1 :: Int)
