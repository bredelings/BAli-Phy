{-# LANGUAGE NoImplicitPrelude #-}

data Bool = True | False

not True = False
not False = True

data Maybe a = Nothing | Just a

class Show a where
    show :: a -> [Char]

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

data Eq a => Set a = EmptySet | Num a => Singleton a | Two (Set a) (Set a)

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
    (>>=) :: m a -> (a -> m b) -> m b

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)

instance Eq Bool where
    True == True = True
    False == False = True
    True == False = False
    False == True = False

instance Eq a => Eq [a] where
    (x:xs) == (y:ys) = (y == x) && (xs == ys)
    []     == []     = True
    _      == _      = False

instance Eq Int where
    x == y = True

instance Show Int where
    show x = ""

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

plus x y = x + y

True && True = True
_    && _    = False

listeq (x:xs) (y:ys) = (x == y) && (xs == ys)
listeq []     []     = True
listeq _      _      = False

product [] = 1
product (x:xs) = x * product xs

int_prod = product [1,2] :: Int

