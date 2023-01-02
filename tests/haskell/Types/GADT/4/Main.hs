{-# LANGUAGE GADTs, NoImplicitPrelude #-}

-- Are we somehow turning the metatyvar until a rigid type var
-- during quantification, thus avoiding the skolem-escape scenario?
data I a = I a
data SomeI = forall a. MkSomeI (I a)
unI (MkSomeI i) = i
