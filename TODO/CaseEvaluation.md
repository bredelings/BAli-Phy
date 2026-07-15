# Case evaluation

This note concerns the runtime representation and evaluation of `case`.  In
particular, it asks when a case scrutinee or result should have its own graph
register and how an unallocated case can participate in invalidation.  The
optimizer-facing strict-argument question is in
[StrictArguments](StrictArguments.md); stack representation is in
[Stack](Stack.md).

## Allocation and reuse

Allocating a register for every intermediate case expression is expensive.
Direct evaluation would also separate expression evaluation from register
evaluation and could allow chains such as

    case x of (y, z) -> case a of (b, c) -> builtin z c

to execute without allocating a register for each case continuation.

However, the allocated registers are also cache and dependency-graph nodes.
For example, in

    let z = case x of K p -> p
    in builtin z

the register for `z` remembers both its old and new result.  If `x` changes but
the selected `p` does not, `z` can prevent `builtin z` from running again.
Changing this to `case x of K p -> builtin p` removes that comparison point.
With the current evaluator, entering a branch may also allocate fresh cells for
the remaining case continuation and the builtin.

Case allocation is therefore a granularity choice, not just overhead.  Removing
a case-result register needs a replacement reuse rule or an explicit decision
that recomputation is cheaper than caching.

## Direct expression evaluation

Evaluating `case E of alts` without first allocating a register for `E` needs an
operation like `evaluate(E.exp, E.env)` which records dependencies on the
current step.  Passing the expression and environment separately decouples
expression evaluation from `Reg.exp.object` and `Reg.env`.

If `E` reduces to an index variable, the selected register is a dependent
USE/FORCE edge.  Replacing `E` with a newly constructed index-variable
expression avoids that edge only by moving the same dynamic choice elsewhere.
Direct evaluation should use the existing dependent-edge representation rather
than introduce another dependency mechanism.

Normalization currently replaces case objects with register variables.  Case
evaluation without those registers would require normalization to retain the
expression, or a later phase to move allocation into the case operation.

## Single-alternative case prefixes

A possible first representation is to fuse a prefix of single-alternative
cases with its continuation.  The prefix would evaluate scrutinees, bind fields,
trim the extended environment, and eventually run the final operation.  This
avoids allocating each case and continuation as a separate graph cell.

The prefix can be viewed as a sequence of computable dependency observations:

* a fixed or dependent USE/FORCE of a scrutinee;
* selection and field binding for one alternative;
* trimming the continuation environment; and
* the final operation.

This is more general than an ordinary edge: replaying it performs pattern
matching and environment construction before it can decide whether an input is
unchanged.

## Preserving unchanged results

To recover the filtering supplied by a let-bound case result, a fused operation
could retain the trimmed environment produced by its case prefix.  On
invalidation it would replay the prefix and compare the new live bindings with
the retained environment using the same value-identity rules as USE edges.  If
the environments match, it could retain the final result; otherwise it would
continue evaluation with the new environment.

This makes case prefixes resemble replayable, computed USE/FORCE edges.  A
general replay mechanism would need:

* the old ordered observation trace and a replay cursor;
* the current prefix position and partially constructed environment;
* the number of old demands already reactivated; and
* a rule for discarding and replacing the old suffix after the first mismatch.

The reactivated-prefix state is required so that resuming evaluation does not
increment force counts twice.  The current invalidation path can replay fixed
edges, but conservatively refuses to retain a result when its old step has
dependent edges because it does not track this state.

Multiple alternatives add branch identity and differently shaped environments.
They should not be included in an initial case-prefix design.

## Values in the continuation

Case binders may be lifted pointers or unboxed values.  Current closures only
hold pointers, whereas an evaluator frame could also carry unboxed values.
Extending closures would require either a separate value environment or a
layout that groups pointer fields.  `object_ptr`-like values also need lifetime
and finalizer handling, so they cannot be treated as ordinary unboxed scalars.

## Current conclusion

Do not eliminate case-result registers solely because a case is
single-alternative.  First decide whether fused prefixes should cache and replay
their trimmed environments, and define the required count accounting.  Until
then, the existing cells preserve useful change filtering even when their
allocation looks redundant.
