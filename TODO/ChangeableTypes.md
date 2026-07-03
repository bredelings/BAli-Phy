# Changeable Types

Currently the changeable interpreter treats all cells as possibly changeable.
Cells in that machine can be constant, modifiable, or changeable.  If we can
prove that some computations cannot depend on modifiables, then we should be
able to run them in a faster interpreter.

There are three related ideas that should not be conflated:

* a value may be tainted, meaning that it may depend on modifiables;
* a runtime heap cell may be changeable, meaning that observing it can record
  dependency edges;
* evaluating an expression may have an effect, such as recording USE or FORCE
  edges.

Thus `Changeable a` is ambiguous.  It might mean a tainted `a`, or it might
mean a heap-cell handle whose current value has type `a`.

## Data Types

We need to express which parts of data types are changeable.  For lists, the
spine of the list and the values in the list can have separate taints.  We can
write this as `[a@p]@q`, where `p` says whether the elements are changeable,
and `q` says whether the constructor choices are changeable.

Writing `[a@changeable]@changeable` as `Changeable [Changeable a]` can be a
useful intuition, but it is misleading if `Changeable` means a runtime cell
that changeably evaluates to a pure list.  What we want is that the tail fields
of a changeable list are changeable too.

One possible default is one structural taint per recursive datatype SCC, plus
one taint for each type parameter.  Recursive occurrences inside the SCC inherit
the same structural taint.  Non-recursive ADT fields keep their own taint
summaries; otherwise a wrapper type like `Box [a]` would unnecessarily taint
the list spine merely because the box is tainted.

For lists:

    data List@q a@p = Nil | Cons a@p (List@q a@p)

For mutually recursive types, the SCC can be inferred together:

    data X@shape a@p b@q = NoY | YesY (Y@shape a@p b@q)
    data Y@shape a@p b@q = NoX | YesX (X@shape b@q a@p)

This shares the structural taint through the recursive family while preserving
the taints of type parameters under substitution. We could in theory add
separate taint variables for each type in the SCC -- X@xshape@yshape -- but this
may not be worth it.

## Function Summaries

For standard `take`,

    take 0 xs       = []
    take n []       = []
    take n (x : xs) = x : take (n-1) xs

we would like to infer a summary like:

    take :: Int@n -> [a@p]@q -> [a@p]@(n | q)

The result spine depends on `n` and on the input spine `q`.  The element taint
`p` is preserved, but it does not affect which constructors are produced.

This kind of summary could be inferred by abstract interpretation over typed
Core.  Case scrutinees contribute to the taint of constructor choice, fields
preserve their own taints, and recursive summaries are solved by a fixpoint.

## Effects

Instead of explicitly threading `MachineState` through Core, we can annotate
Core judgments with effects:

    Gamma |- e : tau ! effect

Function arrows may also have latent effects:

    a ->{e} b

The plain arrow `a -> b` can mean `a ->{pure} b`.  This keeps ordinary values
ordinary while still recording that applying some functions may perform reads.

### Effects and Function Bodies

A "Read" effect on a function arrow can say that evaluating a body may record
dependencies.
It does not need to record exactly which arguments are used on each arrow.
The Core body should say which reads occur and where they occur.
This is closer to the usual Haskell style:

    a -> b -> Eval c

where partial application is pure and effects happen when the final computation
is evaluated.

For example, `id` should be taint-polymorphic and pure:

    id :: a@p -> a@p

It changes no machine state and preserves the taint of its argument.  In
contrast, a changeable version of `take` may record reads while evaluating the
result spine:

    take :: Int@n -> [a@p]@q ->{Read} [a@p]@(n | q)

The summary says how taints flow.  The elaborated body, using `read` and
`readcase`, says which dependencies are actually recorded.  In a monadic
style, this would translate to

    take :: Int@n -> [a@p]@q -> Read ([a@p]@(n | q))

If we had a strict version of `take` where `take n` already read `n`, then
we would put a `{Read}` effect on the first arrow as well.  In a monadic style
this would translate to

    take :: Int@n -> Read([a@p]@q -> Read ([a@p]@(n | q)))

This nesting is awkward, and is one reason it is attractive to keep detailed
read placement in the Core body rather than encode it all in arrows.


## Extended Core

One idea is to add a form like:

    read MODE x as x' in body

where `x :: Changeable a` and `x' :: a`.  The read records a dependency edge
and binds an ordinary value, so pure code can use `x'` without recording
additional edges.  This form is most natural for scalar or opaque values where
reading the current value gives a representable ordinary type.

Initially, `read` should probably take only lifted heap variables.  Dependency
edges target heap cells, so arbitrary `read E` would hide allocation and
materialization decisions.  For example, `read (x + y)` could mean reading `x`
and `y`, or it could mean allocating a heap cell for `x + y` and reading that
cell.  Those are different choices.

### Readcase and Constructor Views

Plain `read` is awkward for ADTs with tainted recursive fields.  For example,
reading a value of type `[a@p]@changeable` reveals an ordinary constructor, but
the fields of that constructor can still be tainted.  The simple type
`[a@p]@q` has no good way to say "this constructor layer is known, but the tail
keeps the original spine taint."

One way to avoid that intermediate type is to merge read and case:

    readcase MODE x of
      []     -> ...
      (x:xs) -> ...

The alternative binders have representable types directly.  In the cons branch,
`x :: a@p` and `xs :: [a@p]@q`, where `q` is the original spine taint.
This form deliberately has no case binder for the observed scrutinee.  If a
branch needs the observed whole value, it should reconstruct that value from
the selected constructor and field binders, such as `x:xs` or `Nothing`.

An implementation may still need a value representation for the cached result
of observing a changeable ADT node: the cached constructor is not changeable,
but its fields can carry tainted types such as `a@p` and `[a@p]@q`.  `readcase`
keeps that one-layer view internal instead of forcing it into the surface type
of a separate binder.

### Read Modes

The mode is probably not just USE versus FORCE.  Sampling operations often need
the current value of a parameter, but changes to that parameter should update
the probability of the sampled value rather than replace the sampled value.
This suggests at least:

* `USE`: the value is used, and changes invalidate this result.
* `FORCE_VALUE`: the current value is needed, but changes update probability or
  effects rather than replacing this result.
* `FORCE_ONLY`: the child must be evaluated or current, but its value is not
  used.

### Builtin Operations

If builtin operations cannot take changeable arguments, then:

    op x y

must be repaired to something like:

    read USE x as x' in
    read USE y as y' in
    op x' y'

This is useful because the read is required by the type change from
`Changeable a` to `a`.  It also makes the fast-interpreter boundary explicit:
after the dependency is recorded, ordinary pure code can run on ordinary values.

For example, if `n :: Changeable Int`, then:

    factorial n

could become:

    read USE n as n' in factorial n'

so that only the first read records a dependency, and the recursive arithmetic
runs as pure computation on `n'`.

## No-Replace Scopes

In ordinary lazy evaluation, if a heap cell contains `E` and evaluating `E`
produces `F`, then the cell can often be updated from `E` to `F`.  This is not
correct when the reduction from `E` to `F` is conditional on changeable values:
under different modifiable values, `E` might reduce differently.

A possible notation is:

    noreplace {
      readcase USE z of
        (a,b) -> a + b
    }

The reads are the conditions for the protected reduction.  The protected
expression must not be overwritten by its current result; instead, the result
is represented separately, for example by a called heap cell.  This is different
from merely saying where a new cell begins.  It says which reduction is
conditional and therefore cannot be remembered by destructive update.

For example:

    case x of (y,z) ->
      case z of (a,b) ->
        a + b

might become:

    case x of (y,z) ->
      noreplace {
        readcase USE z of
          (a,b) ->
            noreplace {
              read USE a as a'
              read USE b as b'
              a' + b'
            }
      }

if `x` is stable but `z`, `a`, and `b` are changeable heap values.  The outer
case on `x` can still be replaced normally.  The case on `z` cannot be
replaced by the selected alternative without retaining the condition that `z`
had its current value.  Likewise, the arithmetic cannot be replaced by its
current integer result without retaining the conditions on `a` and `b`.

A working interpretation is that `noreplace` protects the next reducible
operation after its reads.  It also gives an implicit source heap cell for the
reads.  Without such a scope, `readcase USE z` specifies the edge target but
not the cell that owns the edge.  The exact normalized form still needs design.

## Merged Continuations

Some useful programs need dependencies whose target is discovered while
evaluating a protected reduction.  For example:

    lookupMaybe a i =
      noreplace {
        read USE i as i'
        case a ! i' of tmp ->
          merged {
            readcase USE tmp of
              Just x  -> Just x
              Nothing -> Nothing
          }
      }

Here `tmp` is computed from `a ! i'`.  If `tmp` is not materialized as a heap
cell, then `readcase USE tmp` has no stable heap target.  The current machine
would need let-floating or allocation so that `tmp` names a heap cell.  Since
`readcase` has no case binder, returning the observed value means reconstructing
it in each alternative.

The `merged` notation is a possible future extension.  It means that the
selected continuation keeps evaluating in the same protected step, rather than
starting a new called cell immediately.  This would let the step record
additional dependencies discovered after the first read.  Old dynamic edges
would have to be removed and replaced when the step is invalidated and
re-executed.

## Optimization

The goal of making reads explicit is to allow transformations.  For example, if
all case alternatives perform the same read, then the read might be lifted out
of the alternatives:

    case x of
      Nothing -> noreplace { read USE y as y' in y' + 1 }
      Just z  -> noreplace { read USE y as y' in y' + z }

could become:

    noreplace {
      read USE y as y'
      case x of
        Nothing -> y' + 1
        Just z  -> y' + z
    }

This is valid only when the read occurs on every path being crossed, does not
mention binders that would go out of scope, and can be moved without changing
strictness or effect order.  It may also widen the protected reduction and
change caching granularity, so it is not merely a cosmetic transformation.

## Tainted Function Versions

When calling a function like `factorial n` or `take n xs` with changeable
arguments, we should probably delegate handling of the changeability to the
function itself.

This will require multiple different versions of the function depending on
the taint status.  For `factorial n`, there would be a single taint status,
but for:

    take :: Int@n -> [a@p]@q -> [a@p]@(n | q)

there are taint roles for `n`, the element type `p`, and the list spine `q`.
A `read` on `n` can lower the local state from `Changeable Int` to ordinary
`Int`.  A `readcase` on `xs` only observes one constructor layer; in the cons
branch, the tail keeps the original spine taint.

For example, when `n` is changeable but `xs` is pure, the specialized version
could be a wrapper around a less-tainted worker:

    take_Nchg_Xpure n xs =
      read USE n as n' in
      take_Npure_Xpure n' xs

After each read, calls should be resolved against the lowered local taint state.
This matters for recursive calls as well as ordinary calls: a version that reads
`n` should not blindly recurse to the same changeable-`n` version if it is now
passing `n' - 1`.
