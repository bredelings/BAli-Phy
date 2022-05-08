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

class Eq a => Ord a where
    (>) :: a -> a -> Bool
    (<) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (<=) :: a -> a -> Bool

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

f x y = case x == y of True  -> x + y
                       False -> x - y

instance Eq Int
instance Show Int
instance Num Int

(a,b) = ('a',2)       
[x,y] = [1,2]
z = "ab"
[e,c] = "ab"
ff x y = x
g = ff :: a -> Char -> a

