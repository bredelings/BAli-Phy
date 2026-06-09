{-# LANGUAGE NoImplicitPrelude #-}

class C a

data family F a

data instance F Int = forall a. C a => MkF a

use :: C Char => Char -> F Int
use x = MkF x
