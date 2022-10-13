class Foo a where
    foo :: a -> a -> a

instance Foo Int where
    foo = (+)

bar x y = x + (foo y 1)

main = do putStrLn $ show $ bar 2 3
