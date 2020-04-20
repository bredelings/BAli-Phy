module Range where

data Range = OpenInterval Double Double | IntegerInterval Int Int | TrueFalseRange | Simplex Int Double | ListRange [Range] | LabelledSimplex [a] Double | TreeRange Int
 
builtin builtinGetBounds 2 "get_bounds" "Range"
builtin builtinGetIntegerBounds 2 "get_integer_bounds" "Range"
  
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

getBounds (OpenInterval Nothing Nothing)   = builtinGetBounds () ()
getBounds (OpenInterval Nothing (Just u))  = builtinGetBounds () u
getBounds (OpenInterval (Just l) Nothing)  = builtinGetBounds l ()
getBounds (OpenInterval (Just l) (Just u)) = builtinGetBounds l u
getBounds _                                = error "getBounds is undefined if argument is not an OpenInterval!"

getIntegerBounds (IntegerInterval Nothing Nothing)   = builtinGetIntegerBounds () ()
getIntegerBounds (IntegerInterval Nothing (Just u))  = builtinGetIntegerBounds () u
getIntegerBounds (IntegerInterval (Just l) Nothing)  = builtinGetIntegerBounds l ()
getIntegerBounds (IntegerInterval (Just l) (Just u)) = builtinGetIntegerBounds l u
getIntegerBounds _                                = error "getIntegerBounds is undefined if argument is not an IntegerInterval!"

c_range (OpenInterval a b) = getBounds (OpenInterval a b)
c_range (IntegerInterval a b) = getIntegerBounds (IntegerInterval a b)
c_range r = r

