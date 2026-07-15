{-# LANGUAGE NoImplicitPrelude #-}

data Proxy (a :: *) = Proxy

value :: Proxy Int
value = Proxy
