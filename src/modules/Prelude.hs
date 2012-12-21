module Prelude where
{
infixl 9 .;  
infixl 8 ^, ^^, **;
infixl 7 *, /, `div`, `mod`, `rem`, `quot`;
infixl 6 +, -;
infixr 5 ++;
infix 4 ==, /=, <, <=, >, >=, `elem`, `notElem`;
infixr 3 &&;
infixr 2 ||;
infixl 1 >>, >>=;
infixr 0 $, $!, `seq`, `join`;

infixr 9 !!, !;

(f . g) x = f (g x);

id x = x;

flip f x y = f y x;

f $ x = f x;
f $! x = x `seq` f x;

True  && x = x;
False && x = False;
True  || x  = True;
False || x = x;

not True         =  False;
not False        =  True;

otherwise = True;

foldr f z [] = z;
foldr f z (x:xs) = (f x (foldr f z xs));

foldl f z [] = z;
foldl f z (x:xs) = foldl f (f z x) xs;

foldl1 f (x:xs)  =  foldl f x xs;
foldl1 _ []      =  error "Prelude.foldl1: empty list";

foldl' f z [] = z;
foldl' f z (x:xs) = let {z' = (f z x)} in seq z' (foldl' f z' xs);

head (h:_) = h;
head []    = error "Prelude.head: empty list";

tail (_:t) = t;
tail []    = error "Prelude.tail: empty list";

last [x]         =  x;
last (_:xs)      =  last xs;
last []          =  error "Prelude.last: empty list";

null []          =  True;
null (_:_)       =  False;

init [x] = x;
init (x:xs) = x:(init xs);
init []     = error "Prelude.init: empty list";

length []        =  0;
length (_:l)     =  1 + length l;

repeat x = let {xs = x:xs} in xs;

iterate f x = x:iterate f (f x);

replicate n x = take n (repeat x);

cycle []         =  error "Prelude.cycle: empty list";
cycle xs         =  let {xs' = xs ++ xs'} in xs';

take 0 x     = [];
take n []    = [];
take n (h:t) = h:(take (n-1) t);

drop 0 xs     =  xs;
drop _ []     =  [];
drop n (_:xs) =  drop (n-1) xs;

splitAt n xs  =  (take n xs, drop n xs);

reverse          =  foldl (flip (:)) [];

and              =  foldr (&&) True;
or               =  foldr (||) False;

any p            =  or . map p;
all p            =  and . map p;

elem x           =  any (== x);
notElem x        =  all (/= x);

map f []  = [];
map f (h:t) = (f h):(map f t);
  
fmap = map;

[] ++ y = y;
h:t ++ y = h:(t ++ y);

fst (x,y) = x;

snd (x,y) = y;

swap (x,y) = (y,x);

curry f x y = f (x,y);
uncurry f p = f (fst p) (snd p);

undefined = error "Prelude.undefined";

h:t !! 0 = h;
h:t !! i = t !! (i-1);

sum     = foldl (+) 0;
product = foldl (*) 1;

enumFrom x = x:(enumFrom (x+1));
enumFromTo x y = if (x==y) then [x] else x:(enumFromTo (x+1) y);

zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs;
zipWith _ _ _           =  [];

zip = zipWith (,);

concat xs = foldr (++) [] xs;

concatMap f = concat . map f;

listArray n l = mkArray n (\i -> l !! i);

listArray' l = listArray (length l) l;

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

listToVectorInt l = unsafePerformIO $ do {v <- newVectorInt (length l); copyListToVectorInt l v 0; return v};

newString s = IOAction1 builtinNewString s;

setStringIndexInt v i x = IOAction3 builtinSetStringIndexInt v i x;

copyListToString [] v i = return ();
copyListToString (h:t) v i = do {setStringIndexInt v i h ; copyListToString t v (i+1)};

listToString l = unsafePerformIO $ do {v <- newString (length l); copyListToString l v 0; return v};

newVectorDouble s = IOAction1 builtinNewVectorDouble s;
setVectorIndexDouble v i x = IOAction3 builtinSetVectorIndexDouble v i x;
copyListToVectorDouble [] v i = return ();
copyListToVectorDouble (h:t) v i = do { setVectorIndexDouble v i h ; copyListToVectorDouble t v (i+1)};

listToVectorDouble l = unsafePerformIO $ do { v <- newVectorDouble (length l); copyListToVectorDouble l v 0; return v};

newVectorMatrix s = IOAction1 builtinNewVectorMatrix s;
setVectorIndexMatrix v i x = IOAction3 builtinSetVectorIndexMatrix v i x;
copyListToVectorMatrix [] v i = return ();
copyListToVectorMatrix (h:t) v i = do { setVectorIndexMatrix v i h; copyListToVectorMatrix t v (i+1)};

listToVectorMatrix l = unsafePerformIO $ do { v <- newVectorMatrix (length l); copyListToVectorMatrix l v 0 ; return v};

error m = builtinError (listToString m);

unsafePerformIO' (IOAction1 x y ) = x y;
unsafePerformIO' (IOAction2 x y z) = x y z;
unsafePerformIO' (IOAction3 x y z w) = x y z w;
unsafePerformIO' (IOAction4 x y z w u) = x y z w u;
unsafePerformIO' (IOReturn x) = x;
unsafePerformIO' (IOAndPass f g) = let {x = unsafePerformIO' f} in x `join` (unsafePerformIO' (g x));
unsafePerformIO' (IOAnd f g) = (unsafePerformIO' f) `join` (unsafePerformIO' g);

unsafePerformIO x = reapply unsafePerformIO' x;

f >> g = IOAnd f g;
f >>= g = IOAndPass f g;
return f = IOReturn f;
fail e = error e
}
