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
    (x:xs) == (y:ys) = y == x && xs == ys
    []     == []     = True
    _      == _      = False

instance Eq Int
instance Show Int
instance Num Int

instance Eq Double
instance Show Double
instance Num Double

plus x y = x + y

True && True = True
_    && _    = False

product [] = 1
product (x:xs) = x * product xs

-- f :: Int -> Int -> Int
f :: Num a => a -> a -> a
f x y = g y y
g x 0 = f x x
g x y = g y x

genericLength :: Num a => [b] -> a
genericLength [] = 0
genericLength (x:xs) = 1 + genericLength xs
-- This should have type Num a => a, but it actually gets type Int
len1 = genericLength "Hello"
len2 = (2 * len1) :: Double

x :: a -> a
(x,y) = (\z -> z, x 1)       
