{-# LANGUAGE NoImplicitPrelude #-}

class C a

data family F a

data instance forall a. C a => F Int = MkF a

use :: C Char => Char -> F Int
use x = MkF x
