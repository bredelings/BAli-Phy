{-# LANGUAGE DefaultSignatures, NoImplicitPrelude, TypeFamilies #-}

class Identity a where
    type Raw a
    type Raw a = a

    convert :: Raw a -> a
    default convert :: Raw a ~ a => Raw a -> a
    convert value = value

data Box = Box

instance Identity Box

box :: Box
box = convert Box

class Weak a
class Weak a => Strong a

class Contextual a where
    contextual :: Strong b => a -> b -> a
    default contextual :: Weak b => a -> b -> a
    contextual value _ = value

instance Contextual Box

contextBox :: Strong b => b -> Box
contextBox = contextual Box

data Wrapped = Wrapped
data RawWrapped = RawWrapped

instance Identity Wrapped where
    type Raw Wrapped = RawWrapped
    convert RawWrapped = Wrapped

wrapped :: Wrapped
wrapped = convert RawWrapped
