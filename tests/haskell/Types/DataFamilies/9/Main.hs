{-# LANGUAGE NoImplicitPrelude #-}

class Renderable a

data Web = Web

data family Component x

data instance forall a. Renderable a => Component Web = MkComponent a
