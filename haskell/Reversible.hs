module Reversible where

data Reversibility = NonEq | EqNonRev | EqRev

instance Eq Reversibility where
    EqRev    == EqRev    = True
    EqNonRev == EqNonRev = True
    NonEq    == NonEq    = True
    _        == _        = False

instance Ord Reversibility where
    compare NonEq    NonEq    = EQ
    compare NonEq    _        = LT

    compare EqNonRev NonEq    = GT
    compare EqNonRev EqNonRev = EQ
    compare EqNonRev EqRev    = LT

    compare EqRev    EqRev    = EQ
    compare EqRev    _        = GT


class CheckReversible m where
    getReversibility :: m -> Reversibility 

class CheckReversible m => CanMakeReversible m where
    setReversibility :: m -> Reversibility -> m

isReversible m = getReversibility m == EqRev
isStationary m = getReversibility m /= NonEq

reversible m = setReversibility m EqRev
stationary m = setReversibility m EqNonRev

