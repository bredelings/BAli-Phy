{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative.Builder.Internal
    ( OptionFields(..)
    , FlagFields(..)
    , ArgumentFields(..)
    , CommandFields(..)
    , HasName(..)
    , HasValue(..)
    , HasMetavar(..)
    , DefaultProp(..)
    , Mod
    , fieldMod
    , defaultMod
    , propertyMod
    , applyMod
    ) where

import Compiler.Base
import Compiler.Classes
import Control.Applicative
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Options.Applicative.Types

-- Compatibility note: upstream Builder.Internal has additional modifier
-- classes and completion, grouping, and style properties still to be ported.
data OptionFields a = OptionFields [OptName] (String -> ParseError)
data FlagFields a = FlagFields [OptName]
data ArgumentFields a = ArgumentFields
data CommandFields a = CommandFields [(String, ParserInfo a)]

class HasName f where
    addName :: OptName -> f a -> f a

instance HasName OptionFields where
    addName name (OptionFields names no_arg_error) = OptionFields (name:names) no_arg_error

instance HasName FlagFields where
    addName name (FlagFields names) = FlagFields (name:names)

class HasValue f where
    hasValueDummy :: f a -> ()

instance HasValue OptionFields where
    hasValueDummy _ = ()

instance HasValue ArgumentFields where
    hasValueDummy _ = ()

class HasMetavar f where
    hasMetavarDummy :: f a -> ()

instance HasMetavar OptionFields where
    hasMetavarDummy _ = ()

instance HasMetavar ArgumentFields where
    hasMetavarDummy _ = ()

instance HasMetavar CommandFields where
    hasMetavarDummy _ = ()

data DefaultProp a = DefaultProp (Maybe a) (Maybe (a -> String))

instance Semigroup (DefaultProp a) where
    DefaultProp value1 show1 <> DefaultProp value2 show2 =
        DefaultProp (value1 <|> value2) (show1 <|> show2)

instance Monoid (DefaultProp a) where
    mempty = DefaultProp Nothing Nothing

-- Keep typed defaults outside OptProperties so parser structure determines whether a field is required.
data Mod f a = Mod (f a -> f a) (DefaultProp a) (OptProperties -> OptProperties)

instance Semigroup (Mod f a) where
    Mod fields1 defaults1 props1 <> Mod fields2 defaults2 props2 =
        Mod (fields2 . fields1) (defaults2 <> defaults1) (props2 . props1)

instance Monoid (Mod f a) where
    mempty = Mod id mempty id

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty id

defaultMod :: DefaultProp a -> Mod f a
defaultMod defaults = Mod id defaults id

propertyMod :: (OptProperties -> OptProperties) -> Mod f a
propertyMod f = Mod id mempty f

applyMod :: Mod f a -> f a -> OptProperties -> (f a, DefaultProp a, OptProperties)
applyMod (Mod modify_fields defaults modify_properties) fields properties =
    (modify_fields fields, defaults, modify_properties properties)
