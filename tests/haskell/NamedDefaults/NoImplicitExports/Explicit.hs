{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Explicit (module Local, default Choose) where
import Data.Bool (Bool)
import Local
default Choose (Bool)
