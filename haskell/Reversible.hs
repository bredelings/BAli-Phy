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
    getReversibility _ = NonEq

class CheckReversible m => CanMakeReversible m where
    setReversibility :: Reversibility -> m -> m

isReversible m = getReversibility m == EqRev
isStationary m = getReversibility m /= NonEq

data IsEqSame = SameEqs | MaybeDiffEqs

instance Eq IsEqSame where
    SameEqs      == SameEqs      = True
    MaybeDiffEqs == MaybeDiffEqs = True
    _            == _            = False

instance Ord IsEqSame where
    SameEqs      <  MaybeDiffEqs = True
    _            <  _            = False
