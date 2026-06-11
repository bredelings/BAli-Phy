module Data.Unique (
   Unique,
   newUnique,
   hashUnique
 ) where

newtype Unique = Unique Integer deriving (Eq,Ord,Show,Read)

nextUnique :: IORef Integer
nextUnique = unsafePerformIO (newIORef 0)

newUnique = do
  x' <- atomicModifyIORef' nextUnique $ \x -> let x' = x+1 in (x',x')
  return (Unique x')

hashUnique (Unique i) = integerToInt i
