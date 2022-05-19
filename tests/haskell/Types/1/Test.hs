{-# LANGUAGE NoImplicitPrelude #-}

data Bool = True | False

not True = False
not False = True

True && True = True
_    && _    = False

True  || _  = True
False || x  = x

data Maybe a = Nothing | Just a

class Show a where
    show :: a -> [Char]

class Read a where
    read :: [Char] -> a

class (Show a, Eq a) => Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    fromInteger :: Int -> a
    abs :: a -> a
    negate :: a -> a
    signum :: a -> a

data C a => D a = Foo (S a)
type S a = [D a]
class C a where
   bar :: a -> D a -> Bool

data Eq a => Set a = EmptySet
                   | Num a => Singleton a
                   | Two (Set a) (Set a)

class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

class Functor f where
    fmap :: (a->b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a->b) -> f a -> f b
    liftA2 :: (a->b->c) -> f a -> f b -> f c

class Applicative m => Monad m where
    return :: a -> m a
    return x = pure x

    (>>=) :: m a -> (a -> m b) -> m b

class Num a => Fractional a where
    (/) :: a -> a -> a

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)

class Eq a => Ord a where
    (>) :: a -> a -> Bool
    (<) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
--    x < y  = not ( x >= y )
    x > y  = not ( x <= y )
    x >= y = (x > y) || (x == y)
    x <= y = (x < y) || (x == y)

instance Eq Bool where
    True == True = True
    False == False = True
    True == False = False
    False == True = False

instance Eq Int where
    x == y = True

instance Ord Int where
    x < y = True

instance Show Int where
    show x = ""

instance Read Int where
    read x = 0

instance Num Int where
    x + y = x
    x - y = x
    x * y = x
    fromInteger x = x
    abs x = x
    negate x = x
    signum x = 1

instance Enum Int where
    succ x = 0
    pred x = 0
    toEnum x = x
    fromEnum x = x
    enumFrom x = [x]
    enumFromThen x y = [x]
    enumFromTo x y = [x]
    enumFromThenTo x y z = [x]

instance Eq Double where
    x == y = True

instance Ord Double where
    x < y = True

instance Show Double where
    show x = ""

instance Read Double where 
    read x = 0

instance Num Double where
    x + y = x
    x - y = x
    x * y = x
    fromInteger x = 0
    abs x = x
    negate x = x
    signum x = 1

instance Fractional Double where
    x / y = 0

--instance Fractional Double where
--    x / y = 0

instance Eq a => Eq [a] where
    (x:xs) == (y:ys) = (y == x) && (xs == ys)
    []     == []     = True
    _      == _      = False

instance (Eq a,Eq b) => Eq (a,b) where
    (x1,y1) == (x2,y2) = (x1 == x2) && (y1 == y2)

class Container c where
    empty :: c a
    insert :: a -> c a -> c a
    contains :: Eq a => a -> c a -> Bool
    asList :: c a -> [a]

fst :: (a,b) -> a             
fst (x,y) = x

ten = [1..10]

-- f,a :: Int->Int->Int
            
-- (a,b) = h 1 2
-- h x   = f x a

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

plus x y = x + y

head (x:xs) = x

tail (x:xs) = xs              

wirp = head [1,2,3]
zurfel = 1
zurfel_int = 1 :: Int

f x 1 = x
f x y = g x y
g x y = f x y

d = let dict=(f,g)
    in fst dict 1 2

(a,b) = ('a',2)       
[x,y] = [1,2]
z = "ab"
[e,c] = "ab"

-- I guess this is basically an instance for (==) for Eq [a].
-- We would put something like this into the dictionary.
-- Interestingly, the (xs == ys) should evaluate to (listeq xs ys)...
--  hopefully the optimizer can figure that out.
-- listeq :: Eq a => [a] -> [a] -> Bool
listeq (x:xs) (y:ys) = (x == y) && (xs == ys)
listeq []     []     = True
listeq _      _      = False

listeq2 :: Eq a => [a] -> [a] -> Bool
listeq2 (x:xs) (y:ys) = (x == y) && (listeq2 xs ys) 
listeq2 []     []     = True
listeq2 _      _      = False

ifelse :: Bool -> a -> a -> a
ifelse x y z = if x then y else z

product [] = 1
product (x:xs) = x * product xs

int_prod = product [1,2] :: Int

--ff c i | i > 10    = c
--       | True      = gg c 'b'
--
--gg 'a' w = ff 'b' 10
--gg z   w = z

h = let f c i | i > 10    = c
              | True      = g c 'b'

        g 'a' w = f 'b' 10
        g z   w = z
      in (f 'a' 1, f 'a' 1.0)

fmap2 f [] = []
fmap2 f (x:xs) = (f x):(fmap2 f xs)
         
zorp x = show (read x + 1)

-- This should fail
-- zorp' x = show (read x)
abc x y = return x == y

genericLength :: Num a => [b] -> a
genericLength [] = 0
genericLength (x:xs) = 1 + genericLength xs
-- this gets a monomorphic type Num a => a (without the forall!)
len1 = genericLength "Hello"
-- this second definition then resolves the a from len1 - weird!
len2 = (2 * len1) :: Double

instance (Eq a, Eq b) => Eq (a,b) where
    (x1,y1) == (x2,y2) = (x1 == x2) && (y1 == y2)

instance (Ord a, Ord b) => Ord (a,b) where
   (x1,y1) < (x2,y2) = (x1 < x2) || ((x1 == x2) && (y1 < y2))

gyon = if (1,4) < (2,3) then "yes" else "no"

