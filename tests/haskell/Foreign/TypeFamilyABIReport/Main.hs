{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base (String)
import Compiler.IO (IO)
import Foreign.String (CPPString)

type family PureABI a where
    PureABI Int = Int -> Int -> Int

foreign import ecall "Prelude:div_int"
    familyDiv :: PureABI Int

type family EffectfulABI a where
    EffectfulABI Int = CPPString -> IO CPPString

foreign import bpcall "Environment:getEnvRaw"
    familyGetEnv :: EffectfulABI Int

foreign import trcall "Environment:getEnvRaw"
    translatedGetEnv :: String -> IO String
