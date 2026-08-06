{-# LANGUAGE NoImplicitPrelude #-}
#if FEATURE && VALUE == 3 && AT_LEAST(4,1,0) && !defined(REMOVED)
#define SELECTED 1
#else
#error command-line CPP configuration was not applied
#endif
module Main where

-- This fixture checks the command-line macro environment, forced preprocessing,
-- exact dumped source, and the absence of macro expansion in Haskell bodies.
data AT_LEAST = AT_LEAST
data SELECTED = SELECTED
