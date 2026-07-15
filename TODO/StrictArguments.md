# Strict arguments

This note concerns how strictness of builtin arguments is declared and how the
optimizer may use it.  Runtime execution and reuse of case continuations is
covered by [CaseEvaluation](CaseEvaluation.md).

## Demand signatures

The optimizer needs a low-effort way to discover which builtin arguments are
demanded.  E-ops normally demand all operands.  For non-e-op builtins, an
explicit strict declaration, such as `strict_function_name`, is preferable to
assuming that every builtin is strict and listing exceptions.  Until the
builtins have been audited, an ordinary `builtin_function_name` should carry no
strictness promise.

A strict argument promise means that evaluating the call requires the
argument's value.  It does not by itself specify:

* whether the runtime records a USE or FORCE edge;
* the order in which strict arguments are evaluated; or
* whether moving the call into a case continuation is profitable.

Most pure builtins and e-ops should not care about the order in which demanded
arguments are evaluated.  Preserving current left-to-right behavior may be
useful during migration, but should not silently become a semantic guarantee.
Builtins that register effects or otherwise expose evaluation order need a
separate audit before receiving a signature that permits reordering.

## USE and FORCE

Demand signatures may not need to expose the runtime's USE/FORCE distinction,
but transformations must preserve the boundary that distinction creates.  For
example, consider

    sampleForce (case x of K p -> p)

The allocated argument can form

    sampleForce --FORCE--> argument case --USE--> x

The case cell observes changes to `x`, while the FORCE edge does not by itself
make a changed case result decide whether `sampleForce` is retained.  Rewriting
the call as

    case x of K p -> sampleForce p

removes that intermediate boundary.  A change to `x` may now recreate or rerun
the sampling continuation even when `p` is unchanged.  This is a consequence
of the graph representation, not evidence that the argument was semantically
lazy.

## Strict-argument transformation

GHC's Core preparation can turn a demanded argument into a case:

    f E  ==>  case E of x -> f x

In an ordinary lazy runtime this can avoid allocating a thunk.  In BAli-Phy's
incremental graph, the removed cell may also have cached a projection and
filtered changes.  With the current evaluator, nested cases can additionally
allocate new cells for their branch continuations.  Moving several arguments
under cases can therefore increase recomputation or allocation even though it
removes the original argument cells.

The semantic demand analysis and this transformation should be separate.  It
is useful to discover and record strict arguments before there is a general
strict-argument rewrite.  Consuming that information should initially be
limited to transformations that retain the current graph boundaries, unless a
local cost argument establishes that recomputation is preferable.

## Dependency on case evaluation

Single-alternative case prefixes might eventually make strict-argument
rewrites cheaper by evaluating cases and their continuation as one operation.
They do not solve reuse automatically: the operation must replay the cases and
compare the resulting trimmed environment before deciding that its old result
is reusable.  The representation and count-accounting requirements are
described in [CaseEvaluation](CaseEvaluation.md).

Until that design is implemented, strictness metadata should not be treated as
permission to perform GHC-style case conversion throughout Core.
