{-# LANGUAGE NoImplicitPrelude #-}

data Bool = True | False

not True = False
not False = True
          
True && True = True
_    && _    = False

True  || _  = True
False || x  = x

data Maybe a = Nothing | Just a

class Functor f where
    fmap :: (a->b) -> f a -> f b

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = (f x):(fmap f xs)

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)

instance Eq a => Eq [a] where
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    []     == []     = True
    _      == _      = False

instance Eq (Maybe a) where
    Just x  == Just y  = x == y
    Nothing == Nothing = True
    _       == _       = False

class Eq a => Ord a where
    (<) :: a -> a -> Bool

instance (Eq a, Eq b) => Eq (a,b) where
    (x1,y1) == (x2,y2) = (x1 == x2) && (y1 == y2)

-- instance (Ord a, Ord b) => Ord (a,b) where
--    (x1,y1) < (x2,y2) = (x1 < x2) || ((x1 == x2) && (y1 < y2))

