module SModel.Property where

import SModel.Rate (RateModel, rate)
import SModel.Simple
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.JSON as J    

newtype StateProperties a = StateProperties [a]

newtype ComponentStateProperties a = ComponentStateProperties [StateProperties a]

type Property = ComponentStateProperties Double

type PropertyMap = Map Text Property

-- A state property function receives additional pure scaling applied after the
-- property was installed. Scale-invariant properties ignore this argument.
type StatePropertyFunction = Double -> StateProperties Double

type StatePropertyMap = Map Text StatePropertyFunction

type EvaluatedStatePropertyMap = Map Text (StateProperties Double)

class HasProperties t m where
    getProperties :: SModelOnTree t m -> PropertyMap

class HasStateProperties m where
    getStatePropertyFunctions :: m -> StatePropertyMap
    setStateProperty :: Text -> StatePropertyFunction -> m -> m
    nPropertyStates :: m -> Int

getPropertyComponents (ComponentStateProperties sps) = length sps
getPropertyStatesForComponent (ComponentStateProperties csps) c = length sps
    where StateProperties sps = csps!!c
getPropertyForComponentState (ComponentStateProperties csps) c s = sps!!s
    where StateProperties sps = csps!!c

getComponentStateProperties (ComponentStateProperties csps) = csps

instance Functor StateProperties where
    fmap f (StateProperties ps) = StateProperties (fmap f ps)

instance Functor ComponentStateProperties where
    fmap f (ComponentStateProperties csps) = ComponentStateProperties (fmap (fmap f) csps)

{- Suppose we use a function plus extents
   Then each component could have a different number of states.
   So we'd have ComponentStateProperties Int (Int -> Int) (Int -> Int -> Double)
 -}

ratePropertyName = Text.pack "rate"

dNdSPropertyName = Text.pack "dNdS"

posSelectionPropertyName = Text.pack "posSelection"

constantStateProperties n x = StateProperties $ replicate n x

singletonComponentProperty ps = ComponentStateProperties [ps]

appendComponentStateProperties ps = ComponentStateProperties $ concat [csps | ComponentStateProperties csps <- ps]

getStateProperties :: HasStateProperties m => m -> EvaluatedStatePropertyMap
getStateProperties = Map.map (\property -> property 1) . getStatePropertyFunctions

scaleStateProperty :: Double -> StatePropertyFunction -> StatePropertyFunction
scaleStateProperty scale property = \scale2 -> property (scale * scale2)

scaleStatePropertyMap :: Double -> StatePropertyMap -> StatePropertyMap
scaleStatePropertyMap scale = Map.map (scaleStateProperty scale)

-- Install a property whose value is the same for every state in the model.
setConstantStateProperty :: HasStateProperties m => Text -> Double -> m -> m
setConstantStateProperty name value model = setStateProperty name property model
    where n = nPropertyStates model
          property _ = constantStateProperties n value

-- Store the model rate as a delayed property so later scaling changes the value.
setRateProperty :: (HasStateProperties m, RateModel m) => m -> m
setRateProperty model = setStateProperty ratePropertyName property model
    where n = nPropertyStates model
          baseRate = rate model
          property scale = constantStateProperties n (scale * baseRate)

statePropertyMapToComponentPropertyMap = Map.map singletonComponentProperty

-- Keep only property names that every component provides, and concatenate the
-- corresponding component-state values in the mixture order.
commonPropertyMap [] = Map.empty
commonPropertyMap (properties:rest) = Map.fromList
    [ (name, appendComponentStateProperties (property:[p Map.! name | p <- rest]))
    | (name, property) <- Map.toAscList properties
    , all (Map.member name) rest
    ]

markovModulateProperty (ComponentStateProperties csps) = StateProperties $ concat [ ps | StateProperties ps <- csps]

instance J.ToJSON a => J.ToJSON (ComponentStateProperties a) where
    toJSON (ComponentStateProperties csps) = J.toJSON [ sps | StateProperties sps <- csps]
    toEncoding (ComponentStateProperties csps) = J.toEncoding [ sps | StateProperties sps <- csps]
