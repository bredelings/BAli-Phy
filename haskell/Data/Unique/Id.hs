module Data.Unique.Id (
    Id,
    hashedId,
    IdSupply,
    initIdSupply,
    splitIdSupplyL,
    splitIdSupply,
    idFromSupply
  ) where

data Id = Id { hashedId :: Int }

instance Eq Id where
    Id x == Id y  =  x == y

instance Ord Id where
    Id x  < Id y  =  x  < y

instance Show Id where
    show (Id x) = "Id " ++ show x

data IdSupply = IdSupply Char Int IdSupply IdSupply

initIdSupply :: Char -> IO IdSupply
initIdSupply c = let mkSupply = unsafeInterleaveIO $ do
                                  i <- nextInt
                                  l <- mkSupply
                                  r <- mkSupply
                                  return $ IdSupply c i l r
                 in mkSupply

splitIdSupplyL :: IdSupply -> [IdSupply]
splitIdSupplyL ids = l:splitIdSupplyL r
    where (l,r) = splitIdSupply ids

splitIdSupply :: IdSupply -> (IdSupply, IdSupply)
splitIdSupply (IdSupply _ _ l r) = (l, r)

idFromSupply :: IdSupply -> Id
idFromSupply (IdSupply _ i _ _) = Id i

source :: IORef Int
source = unsafePerformIO (newIORef 0)

nextInt :: IO Int
nextInt = do
  n <- readIORef source
  writeIORef source (succ n)   -- I think this is supposed to throw an exception
  return n                     -- if it wraps, but currently does not.

