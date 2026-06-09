{-# LANGUAGE NoImplicitPrelude #-}

import System.IO (putStrLn)

class Renderable a where
    render :: a -> [Char]

data Web = Web
data Label = Label

instance Renderable Label where
    render Label = "label"

data family Component x

data instance Component Web = forall a. Renderable a => MkComponent a

drawComponent :: Component Web -> [Char]
drawComponent (MkComponent item) = render item

main = putStrLn (drawComponent (MkComponent Label))
