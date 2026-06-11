module Reversible where

data Reversibility = NonEq | EqNonRev | EqRev deriving (Eq, Ord, Read, Show)

class CheckReversible m where
    getReversibility :: m -> Reversibility
    getReversibility _ = NonEq

class CheckReversible m => CanMakeReversible m where
    setReversibility :: Reversibility -> m -> m

isReversible m = getReversibility m == EqRev
isStationary m = getReversibility m /= NonEq

data IsEqSame = SameEqs | MaybeDiffEqs deriving (Eq, Ord, Read, Show)
