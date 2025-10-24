module Reversible where

data Reversibility = EqRev | EqNonRev | NonEq

instance Eq Reversibility where
    EqRev    == EqRev    = True
    EqNonRev == EqNonRev = True
    NonEq    == NonEq    = True
    _        == _        = False

instance Ord Reversibility where
    compare EqRev    EqRev    = EQ
    compare EqRev    _        = LT

    compare EqNonRev EqRev    = GT
    compare EqNonRev EqNonRev = EQ
    compare EqNonRev NonEq    = LT

    compare NonEq    NonEq    = EQ
    compare NonEq    _        = GT

class CheckReversible m where
    getReversibility :: m -> Reversibility

class CheckReversible m => CanMakeReversible m where
    setReversibility :: m -> Reversibility -> m

isReversible m = getReversibility m == EqRev
isStationary m = getReversibility m /= NonEq

reversible m = setReversibility m EqRev
stationary m = setReversibility m EqNonRev

data IsEqSame = SameEqs | MaybeDiffEqs

instance Eq IsEqSame where
    SameEqs      == SameEqs      = True
    MaybeDiffEqs == MaybeDiffEqs = True
    _            == _            = False

instance Ord IsEqSame where
    SameEqs      <  MaybeDiffEqs = True
    _            <  _            = False
