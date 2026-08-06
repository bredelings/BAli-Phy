{-# LANGUAGE NoImplicitPrelude #-}
module Main where

-- CPP is opt-in: without the CPP pragma, directives remain ordinary Haskell
-- input and must be rejected by the parser rather than silently interpreted.
#if 1
data Enabled = Enabled
#endif
