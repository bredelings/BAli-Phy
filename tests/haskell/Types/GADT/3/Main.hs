{-# LANGUAGE GADTs #-}

data T a = Eq a => MkT a

check (MkT x) = x == x

data S = forall x. Eq x => MkS x

check2 (MkS x) = x == x
