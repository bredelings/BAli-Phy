import Data.Map as Map

main = do
  let m = Map.fromList [("A",2),("B",7)]
  putStrLn (show $ Map.keys m)
  putStrLn (show $ Map.elems m)
  putStrLn (show $ Map.assocs m)
  putStrLn (show $ Map.keySet m)
  putStrLn (show $ m!?"C")
