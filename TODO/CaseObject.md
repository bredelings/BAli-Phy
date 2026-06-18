# Avoiding allocating when evaluating case E of alts

Currently, if E is not a variable we float it and replace it with a variable.
When E is not changeable this is pointless.
When E is changeable, if it evaluates to a non-variable, we can just perform the alts.
When E is changeable and evaluates to a variable, we currently have no way of recording
a [dependent use](DependantUse.md).

## Avoiding dependent uses

One thing we could do _without_ dependant uses, is to evaluate
to `case x of alts`, where `x` is the result of evaluate E.
This avoids marking any dependent uses on the first reg.
The called reg then uses `x`, which is OK.  Not sure if this
would actually be faster though.

I guess one this things requires is the ability to evaluate
E even when E isn't the closure for a reg r.
