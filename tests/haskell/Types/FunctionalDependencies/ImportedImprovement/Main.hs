{-# LANGUAGE NoImplicitPrelude #-}

import Support

data R = R

class Q a where
    consume :: a -> R

instance Q A where
    consume _ = R

improvedFromImportedInstance :: R
improvedFromImportedInstance = consume (convert D)
