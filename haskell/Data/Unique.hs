module Data.Unique (
   Unique,
   newUnique,
   hashUnique
 ) where

data Unique = Unique Integer

nextUnique :: IORef Integer
nextUnique = unsafePerformIO (newIORef 0)

newUnique = do
  x' <- atomicModifyIORef' nextUnique $ \x -> let x' = x+1 in (x',x')
  return (Unique x')

hashUnique (Unique i) = integerToInt i

instance Eq Unique where
    (Unique i) == (Unique j)  =  (i==j)

instance Ord Unique where
    (Unique i) < (Unique j)   = (i<j)
