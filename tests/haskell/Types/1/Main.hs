module Main where

polyapply :: (forall a.a->a) -> (Int, Char)
polyapply f = (f 1, f 'a')

polyapply2 (f :: forall a.a->a) = (f 1, f 'a')

main = do putStrLn "HelloWorld!"
          putStrLn $ show [[1],[2]]
          putStrLn $ show (1,'a')
          putStrLn $ show 'a'
          putStrLn $ show $ polyapply (id :: forall b.b->b)
          putStrLn $ show $ polyapply id
          putStrLn $ show $ polyapply2 (id :: forall b.b->b)
          putStrLn $ show $ polyapply2 id

