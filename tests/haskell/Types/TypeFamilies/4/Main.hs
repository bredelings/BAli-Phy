{-# LANGUAGE NoImplicitPrelude #-}

{- This test creates:

   [W] C a
   [W] a ~ (Arg a -> Res a)

Processing the first wanted creates two regular metatypevar's to break the cycle:

   [W] a ~ (cbv1 -> cbv2)
   [W] C a
   [W] cbv1 ~ Arg a
   [W] cbv2 ~ Res a
 
We then immediately continue processing [W] a ~ (cbv1 -> cbv2) and substitute for a, yielding:

   [W] C (cbv1 -> cbv2)
   [W] cbv1 ~ Arg (cbv1 -> cbv2)
   [W] cbv2 ~ Res (cbv1 -> cbv2)

The first wanted can then be satisfied via the class instance for C.
The remaining wanteds are reduced to reflexivites via the type instances for Arg and Res.

-}

type family Arg a
type family Res a

type instance Arg (c -> d) = c
type instance Res (c -> d) = d

class C a where
    method :: a -> a

instance C (a -> b) where
    method x = x

foo :: (a ~ (Arg a -> Res a)) => a -> a
foo x = x

func x = method (foo x)
