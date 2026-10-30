{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module All (default Choose, default Other) where
import Data.Bool (Bool)
import Base
type Boolean = Bool
default Choose (Boolean, Char)
default Other (Bool)
