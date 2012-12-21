module Prelude where
{
newVectorMatrix s = IOAction1 builtinNewVectorMatrix s;

setVectorIndexMatrix v i x = IOAction3 builtinSetVectorIndexMatrix v i x;

copyListToVectorMatrix [] v i = return ();
copyListToVectorMatrix (h:t) v i = do { setVectorIndexMatrix v i h; copyListToVectorMatrix t v (i+1)};

listToVectorMatrix l = unsafePerformIO (do { v <- newVectorMatrix (length l); copyListToVectorMatrix l v 0 ; return v});

error m = builtinError (listToString m);

fail e = error e;
}