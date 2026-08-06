{-# LANGUAGE CPP, NoImplicitPrelude #-}
#define MIN_VERSION_base(a,b,c) ((a) * 10000 + (b) * 100 + (c) >= 40100)
#if MIN_VERSION_base(4,1,0)
{-# LANGUAGE UnicodeSyntax #-}
#else
{-# LANGUAGE UnknownInactiveExtension #-}
#endif
module Main where

-- This fixture protects loader phase ordering, line preservation, layout, and
-- the Stage-1 rule that directive definitions do not expand Haskell body text.
#define BODY_NAME 1
data BODY_NAME = BODY_NAME

selected ∷ BODY_NAME
selected =
#if MIN_VERSION_base(4,1,0)
    BODY_NAME
#else
    invalid {
#endif
