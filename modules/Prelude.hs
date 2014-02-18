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

x ^ 0 = 1;
x ^ 1 = x;
x ^ n = x*(x^(n-1));

data Bool = True | False;
data Maybe a = Just a | Nothing;
data IO a = IOAction1 (b->a) a | 
            IOAction2 (b->c->a) b c | 
            IOAction3 (b->c->d->a) b c d | 
            IOAction4 (b->c->d->e->a) b c d e |
            IOReturn a |
            IOAndPass (IO b) (b -> IO a) |
            IOAnd (IO b) (IO a);

True  && x = x;
False && x = False;
True  || x  = True;
False || x = x;

not True         =  False;
not False        =  True;

otherwise = True;

builtin log 1 "log" "Prelude";
builtin sqrt 1 "sqrt" "Prelude";
builtin truncate 1 "truncate" "Prelude";
builtin ceiling 1 "ceiling" "Prelude";
builtin floor 1 "floor" "Prelude";
builtin round 1 "round" "Prelude";
builtin builtin_vector_from_list 1 "vector_from_list" "Prelude";
builtin doubleToInt 1 "doubleToInt" "Prelude";
builtin ** 2 "pow" "Prelude";
builtin mod 2 "mod" "Prelude";
builtin builtinError 1 "builtinError";
builtin seq 2 "seq";
builtin join 2 "join";
builtin reapply 2 "reapply";
builtin arraySize 1 "arraySize" "Array";
builtin ! 2 "getIndex";
builtin mkArray 2 "mkArray";
builtin intToDouble 1 "intToDouble";
builtin negate 1 "negate" "Prelude";
builtin exp 1 "exp" "Prelude";
builtin doubleToLogDouble 1 "doubleToLogDouble";
builtin + 2 "add" "Prelude";
builtin - 2 "subtract" "Prelude";
builtin / 2 "divide" "Prelude";
builtin * 2 "multiply" "Prelude";
builtin == 2 "equals" "Prelude";
builtin /= 2 "notequals" "Prelude";
builtin > 2 "greaterthan" "Prelude";
builtin >= 2 "greaterthanorequal" "Prelude";
builtin < 2 "lessthan" "Prelude";
builtin <= 2 "lessthanorequal" "Prelude";
builtin iotaUnsigned 1 "iotaUnsigned";
builtin builtin_set_vector_index 3 "set_vector_index" "Vector";
builtin builtin_new_vector 1 "new_vector" "Vector";
builtin get_vector_index 2 "get_vector_index" "Vector";
builtin vector_size 1 "vector_size" "Vector";
builtin sizeOfVectorUnsigned 1 "sizeOfVectorUnsigned" "Vector";
builtin sizeOfVectorInt 1 "sizeOfVectorInt" "Vector";
builtin sizeOfVectorVectorInt 1 "sizeOfVectorVectorInt" "Vector";
builtin sizeOfVectorvectorInt 1 "sizeOfVectorvectorInt" "Vector";
builtin getVectorIntElement 2 "getVectorIntElement" "Vector";
builtin getVectorVectorIntElement 2 "getVectorVectorIntElement" "Vector";
builtin getVectorvectorIntElement 2 "getVectorvectorIntElement" "Vector";
builtin getStringElement 2 "getStringElement" "Vector";
builtin sizeOfString 1 "sizeOfString" "Vector";
builtin builtinNewVectorInt 1 "NewVectorInt" "Vector";
builtin builtinSetVectorIndexInt 3 "SetVectorIndexInt" "Vector";
builtin builtinNewString 1 "NewString" "Vector";
builtin builtinSetStringIndexInt 3 "SetStringIndex" "Vector";
builtin builtinNewVectorDouble 1 "NewVectorDouble" "Vector";
builtin builtinSetVectorIndexDouble 3 "SetVectorIndexDouble" "Vector";
builtin builtinNewVectorMatrix 1 "NewVectorMatrix" "Vector";
builtin builtinSetVectorIndexMatrix 3 "SetVectorIndexMatrix" "Vector";
builtin vector_Matrix_From_List 1 "Vector_Matrix_From_List" "Vector";

builtin c_fst 1 "c_fst";
builtin c_snd 1 "c_snd";
builtin c_pair' 2 "c_pair";
builtin builtin_show 1 "show";

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

repeat x = xs where {xs = x:xs};

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
_   !! _ = error "Out of bounds list index!";

sum     = foldl (+) 0.0;
product = foldl (*) 1.0;

sumi     = foldl (+) 0;
producti = foldl (*) 1;

enumFrom x = x:(enumFrom (x+1));
enumFromTo x y = if (x==y) then [x] else x:(enumFromTo (x+1) y);

zipWith z (a:as) (b:bs) =  z a b : zipWith z as bs;
zipWith _ _ _           =  [];

zip = zipWith (,);

concat xs = foldr (++) [] xs;

concatMap f = concat . map f;

filter p xs = [ x | x <- xs, p x];

listArray n l = mkArray n (\i -> l !! i);

listArray' l = listArray (length l) l;

list_from_vector' v s i = if (i<s) then (get_vector_index v i):list_from_vector' v s (i+1) else [];

list_from_vector v = list_from_vector' v (vector_size v) 0;

listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):listFromVectorInt' v s (i+1) else [];

listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0;

listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else [];

listFromString v = listFromString' v (sizeOfString v) 0;

listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):listFromVectorVectorInt' v s (i+1) else [];

listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0;

listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):listFromVectorvectorInt' v s (i+1) else [];

listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0;

c_pair (x,y) = c_pair' x y;

pair_from_c p = (c_fst p, c_snd p);

new_vector s = IOAction1 builtin_new_vector s;

set_vector_index v i x = IOAction3 builtin_set_vector_index v i x;

copy_list_to_vector [] v i = return ();
copy_list_to_vector (h:t) v i = do {set_vector_index v i h; copy_list_to_vector t v (i+1)};

slow_list_to_vector l = unsafePerformIO $ do {v <- new_vector (length l); copy_list_to_vector l v 0; return v};
list_to_vector = builtin_vector_from_list;

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
fail e = error e;

min x y = if (x <= y) then x else y;
max x y = if (x >= y) then x else y;

quicksort [] = [];
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
   where { small = [y | y <- xs, y <= x ] ;
           large = [y | y <- xs, y  > x ] };

quicksortWith f [] = [];
quicksortWith f (x:xs) = quicksortWith f small ++ (x : quicksortWith f large)
   where { small = [y | y <- xs, (f y) <= (f x)] ;
           large = [y | y <- xs, (f y)  > (f x)] };
  
show () = "()";
show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")";
show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")";
show [] = "[]";
show (x:y) = "["++show x++show' y++"]" where {show' [] = "";
                                              show' (x:y) = ","++show x++show' y};
show x     = listFromString $ builtin_show x;

sequence [] = return [];
sequence (a:as) = do { x <- a;
                       xs <- sequence as;
                       return (x:xs)
                     };

sequence_ [] = return ();
sequence_ (a:as) = do { a;
                        sequence_ as;
                        return ()
                      };

mapM f = sequence . map f;
mapM_ f = sequence_ . map f;

unzip [] = ([],[]);
unzip [(x,y),l] = ([x:xs],[y:ys]) where {z = unzip l; xs = fst z; ys = snd z}
}
