{-# LANGUAGE NoImplicitPrelude #-}

data Tag = Tag

class C a where
    data D a

data instance D Tag = MkD Tag
