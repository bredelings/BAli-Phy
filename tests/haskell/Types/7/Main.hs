data Foo a = forall b. Show b => Foo b

show_foo (Foo x) = show x

main = do
  putStrLn (show_foo (Foo 1))
