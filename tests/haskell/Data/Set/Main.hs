{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Set as Set
import Data.Function (($))
import Data.Ord
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let xs = Set.fromList [4,1,3,2,3] :: Set.Set Int
      ys = Set.fromList [3,4,5]
      zs = Set.insert 6 $ Set.delete 1 xs
  putStrLn $ show $ Set.toList xs
  putStrLn $ show $ Set.member 3 xs
  putStrLn $ show $ Set.notMember 1 zs
  putStrLn $ show $ Set.toList zs
  putStrLn $ show $ Set.toList $ Set.union xs ys
  putStrLn $ show $ Set.toList $ Set.intersection xs ys
  putStrLn $ show $ Set.toList $ xs Set.\\ ys
  putStrLn $ show $ Set.toList $ Set.map (+10) xs
