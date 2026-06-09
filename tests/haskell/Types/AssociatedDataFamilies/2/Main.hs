{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (putStrLn)

data Tag = Tag

class C a where
    data D a

instance C Tag where
    data D Tag = MkD { getD :: Tag }

useD :: D Tag -> Tag
useD x = getD x

main = case useD (MkD Tag) of
    Tag -> putStrLn "associated data instance ok"
