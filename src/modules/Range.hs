module Range where
{
realLine = OpenInterval Nothing Nothing;

above l = OpenInterval (Just l) Nothing;
below u = OpenInterval Nothing (Just u);
between l u = OpenInterval (Just l) (Just u);

getBounds (OpenInterval Nothing Nothing)   = builtinGetBounds () ();
getBounds (OpenInterval Nothing (Just u))  = builtinGetBounds () u;
getBounds (OpenInterval (Just l) Nothing)  = builtinGetBounds l ();
getBounds (OpenInterval (Just l) (Just u)) = builtinGetBounds l u;
getBounds _                                = ()
}