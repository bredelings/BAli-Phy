module Range where

import Foreign.Maybe

data Range = OpenInterval (Maybe Double) (Maybe Double)
           | IntegerInterval (Maybe Int) (Maybe Int)
           | TrueFalseRange
           | Simplex Int Double
           | ListRange [Range]
           | TreeRange Int
           | NoRange
 
data BuiltinBounds = BuiltinBounds

foreign import bpcall "Range:get_bounds" builtinGetBounds :: CMaybe Double -> CMaybe Double -> BuiltinBounds
foreign import bpcall "Range:get_integer_bounds" builtinGetIntegerBounds :: CMaybe Int -> CMaybe Int -> BuiltinBounds
  
realLine = OpenInterval Nothing Nothing

above l = OpenInterval (Just l) Nothing
below u = OpenInterval Nothing (Just u)
between l u = OpenInterval (Just l) (Just u)

integer_above l = IntegerInterval (Just l) Nothing
integer_below u = IntegerInterval Nothing (Just u)
integer_between l u = IntegerInterval (Just l) (Just u)
all_integers = IntegerInterval Nothing Nothing

logTransform (OpenInterval Nothing _) = error "Can't logTransform ranges with negative numbers!"
logTransform (OpenInterval (Just l) Nothing ) = above (log l)
logTransform (OpenInterval (Just l) (Just u)) = between (log l) (log u)

expTransformRange (OpenInterval Nothing Nothing) = above 0.0
expTransformRange (OpenInterval Nothing (Just u)) = between 0.0 (exp u)
expTransformRange (OpenInterval (Just l) Nothing) = above (exp l)
expTransformRange (OpenInterval (Just l) (Just u)) = between (exp l) (exp u)

getBounds (OpenInterval Nothing Nothing)   = builtinGetBounds cNothing cNothing
getBounds (OpenInterval Nothing (Just u))  = builtinGetBounds cNothing (cJust u)
getBounds (OpenInterval (Just l) Nothing)  = builtinGetBounds (cJust l) cNothing
getBounds (OpenInterval (Just l) (Just u)) = builtinGetBounds (cJust l) (cJust u)
getBounds _                                = error "getBounds is undefined if argument is not an OpenInterval!"

getIntegerBounds (IntegerInterval Nothing Nothing)   = builtinGetIntegerBounds cNothing cNothing
getIntegerBounds (IntegerInterval Nothing (Just u))  = builtinGetIntegerBounds cNothing (cJust u)
getIntegerBounds (IntegerInterval (Just l) Nothing)  = builtinGetIntegerBounds (cJust l) cNothing
getIntegerBounds (IntegerInterval (Just l) (Just u)) = builtinGetIntegerBounds (cJust l) (cJust u)
getIntegerBounds _                                = error "getIntegerBounds is undefined if argument is not an IntegerInterval!"

c_range (OpenInterval a b) = getBounds (OpenInterval a b)
c_range (IntegerInterval a b) = getIntegerBounds (IntegerInterval a b)
c_range _ = error "No c range for other interval types"

