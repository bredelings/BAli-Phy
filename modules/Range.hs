module Range where
{
data Range = OpenInterval Double Double | IntegerInterval Int Int | TrueFalseRange | Simplex Int Double | ListRange [Range];
 
builtin builtinGetBounds 2 "get_bounds" "Range";
  
realLine = OpenInterval Nothing Nothing;

above l = OpenInterval (Just l) Nothing;
below u = OpenInterval Nothing (Just u);
between l u = OpenInterval (Just l) (Just u);

logTransform (OpenInterval Nothing _) = error "Can't logTransform ranges with negative numbers!";
logTransform (OpenInterval (Just l) Nothing ) = above (log l);
logTransform (OpenInterval (Just l) (Just u)) = between (log l) (log u);

expTransform (OpenInterval Nothing Nothing) = above 0.0;
expTransform (OpenInterval Nothing (Just u)) = between 0.0 (exp u);
expTransform (OpenInterval (Just l) Nothing) = above (exp l);
expTransform (OpenInterval (Just l) (Just u)) = between (exp l) (exp u);

getBounds (OpenInterval Nothing Nothing)   = builtinGetBounds () ();
getBounds (OpenInterval Nothing (Just u))  = builtinGetBounds () u;
getBounds (OpenInterval (Just l) Nothing)  = builtinGetBounds l ();
getBounds (OpenInterval (Just l) (Just u)) = builtinGetBounds l u;
getBounds _                                = error "getBounds is undefined if argument is not an OpenInterval!";
}