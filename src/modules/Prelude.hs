module Prelude where
{
listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):listFromVectorInt' v s (i+1) else [];

listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0;

listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else [];

listFromString v = listFromString' v (sizeOfString v) 0;

listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):listFromVectorVectorInt' v s (i+1) else [];

listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0;

listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):listFromVectorvectorInt' v s (i+1) else [];

listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0;

newVectorInt s = IOAction1 builtinNewVectorInt s;

setVectorIndexInt v i x = IOAction3 builtinSetVectorIndexInt v i x;

copyListToVectorInt [] v i = return ();
copyListToVectorInt (h:t) v i = do {setVectorIndexInt v i h; copyListToVectorInt t v (i+1)};

listToVectorInt l = unsafePerformIO (do {v <- newVectorInt (length l); copyListToVectorInt l v 0; return v});

newString s = IOAction1 builtinNewString s;

setStringIndexInt v i x = IOAction3 builtinSetStringIndexInt v i x;

copyListToString [] v i = return ();
copyListToString (h:t) v i = do {setStringIndexInt v i h ; copyListToString t v (i+1)};

listToString l = unsafePerformIO (do {v <- newString (length l); copyListToString l v 0; return v});

newVectorDouble s = IOAction1 builtinNewVectorDouble s;
setVectorIndexDouble v i x = IOAction3 builtinSetVectorIndexDouble v i x;
copyListToVectorDouble [] v i = return ();
copyListToVectorDouble (h:t) v i = do { setVectorIndexDouble v i h ; copyListToVectorDouble t v (i+1)};

listToVectorDouble l = unsafePerformIO (do { v <- newVectorDouble (length l); copyListToVectorDouble l v 0; return v});

newVectorMatrix s = IOAction1 builtinNewVectorMatrix s;
setVectorIndexMatrix v i x = IOAction3 builtinSetVectorIndexMatrix v i x;
copyListToVectorMatrix [] v i = return ();
copyListToVectorMatrix (h:t) v i = do { setVectorIndexMatrix v i h; copyListToVectorMatrix t v (i+1)};

listToVectorMatrix l = unsafePerformIO (do { v <- newVectorMatrix (length l); copyListToVectorMatrix l v 0 ; return v});

error m = builtinError (listToString m);

fail e = error e
}
