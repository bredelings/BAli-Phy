# Changeable Types

Currently the changeable interpreter treats all cells as possibly changeable.
Cells in that machine can be constant, modifiable, or changeable.  If we can
prove that some computations cannot depend on modifiables, then we should be
able to run them in a faster interpreter.

There are three related ideas that should not be conflated:

* a type position may be tainted, meaning that this part of the value may
  depend on modifiables;
* a tainted lifted structural layer is represented by a runtime heap-cell
  handle such as `Changeable a`;
* evaluating an expression may have an effect, such as reading a `Changeable a`
  and recording USE or FORCE edges.

Thus `Changeable a` is not the general notation for a tainted `a`.  It is the
runtime representation used when a lifted structural layer is changeable.  The
source-level taint notation is translated to representation types later.

## Data Types

We need to express which parts of data types are changeable.  For lists, the
spine of the list and the values in the list can have separate taints.  We can
write this abstractly as `[a@p]@q`, where `p` says whether the elements are
changeable, and `q` says whether the structural layer of the list is
changeable.  For a multi-constructor type this includes the constructor choice;
for a single-constructor type it can still include changeable field identities.

Let us assume that the data type `X a b` does not need to track whether its
field types are tainted, because they carry that taint in a self-contained
fashion.

Let us assume that `Changeable a` means a runtime cell that changeably evaluates
to `a`.
`Changeable` has kind `Type -> Type` and can only be applied to lifted values.
So, if `a` is not a heap-allocatable type, then it cannot be changeable.

If we are able to separate Int and Int#, then operations on Int# would be 
unchangeable by definition.  Thus a changeable addition would need to record
a use on the boxed, heap-allocated form, extract the unboxed form, and add it.
This would clearly separate basic operations from edge-recording, which would
be nice.

For recursive data types like list, we need to be able to represent a
non-changeable record with changeable fields.  We can define

    type List@C a = Changeable (List#C a)
    data List#C a = Nil | Cons a (List@C a)
    
However, for data types with no recursive fields, `T#P` and `T#C` would
be identical.

More generally, source-level taint notation is translated to representation
types.  A pure structural layer uses the ordinary representation, while a
changeable structural layer uses a heap cell containing the changeable
representation:

    [[T@P args]] = T#P [[args]]
    [[T@C args]] = Changeable (T#C [[args]])

Here `T#P` may just be the original datatype.  The datatype `T#C` does not need
to know whether its field types are tainted, because the transformed field types
carry that information themselves.

For recursive data types, we propose one "structural" taint per recursive
datatype SCC. 
Recursive occurrences inside the SCC inherit the same (structural) taint.
Non-recursive ADT fields keep their own taint summaries; otherwise a wrapper
type like `Box [a]` would unnecessarily taint the list spine merely because the
box is tainted.

For lists:

    data List@q a@p = Nil | Cons a@p (List@q a@p)

For mutually recursive types, the SCC can be inferred together.  Dropping the
taint markers on type arguments, we would have:

    -- this is the taint-annotated form
    data X@shape a b = NoY | YesY (Y@shape a b)
    data Y@shape a b = NoX | YesX (X@shape b a)

    -- this is the translation to concrete types
    type X@C a b = Changeable (X#C a b)
    type Y@C a b = Changeable (Y#C a b)

    data X#C a b = NoY | YesY (Y@C a b)
    data Y#C a b = NoX | YesX (X@C b a)

This shares the structural taint through the recursive family while preserving
the taints of type parameters under substitution. We could in theory add
separate taint variables for each type in the SCC -- X@xshape@yshape -- but this
may not be worth it.

### Unresolved issues

* What about higher-kinded parameters?

  For a type like `data List f a = Nil | Cons (f a) (List f a)`, then we cannot
  have `Changeable f`, since `f` does not have kind `Type`.  If it unclear if
  this would actually be a problem, since we don't yet know what the tainting
  algorithm on Core function bodies would do here.
  
  To handle this, would we need a type per constructor field?  That is, one for
  the cons (f a) and one for the (List f a)?  If a field is just `a`, then maybe
  the taint could be contained within the `a`.
  
* How about functions?  If we have

        data T a = K (a -> Int)

  would we ever infer a taint on the `a`?

* What about newtypes?

  I think single-constructor data types can still be changeable.  For example, we
  could have
  
        if x > 1 then (p,q) else (x,y)
        
  This is changeable because these two answers point to different fields.
  
  If we have
  
        data X a = X a
        f x = if x > 1 then X p else X q

  then I suppose `X p` and `X q` are still different closures.  But if we have
  
        newtype Y a = Y a
        f x = if x > 1 then Y p else  Y q

  then there is no real `Y` constructor.  So perhaps we should just change `Y@t a`
  to `Y a@t`.

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

The summary says how taints flow.  The elaborated body, using `read`, says
which dependencies are actually recorded.  In a monadic style, this would
translate to

    take :: Int@n -> [a@p]@q -> Read ([a@p]@(n | q))

If we had a strict version of `take` where `take n` already read `n`, then
we would put a `{Read}` effect on the first arrow as well.  In a monadic style
this would translate to

    take :: Int@n -> Read([a@p]@q -> Read ([a@p]@(n | q)))

This nesting is awkward, and is one reason it is attractive to keep detailed
read placement in the Core body rather than encode it all in arrows.


## The concrete `Changeable a` type

To some extent we regard the type `Changeable a` as abstract.

However, it is worth thinking about how we might actually implement the
interpreter in Haskell.  This is pseudo-Haskell.

    data Node = forall a.Node (Changeable a)

    data Step a = Step { used_forced_regs :: Vector Node,
                       , call :: Changeable a 
                       }

    data Changeable a = Constant  { value :: a }
                      | ConstantWithForce  { value :: a
                                           , forced_regs :: Vector Node
                                           }
                      | RegRef           { next :: Changeable a }
                      | RefRefWithForce  { next :: Changeable a
                                         , forced_regs :: Vector Node 
                                         }
                      | Changeable  { exp:: Expr a
                                    , step :: Step a,
                                    , used_by :: Node,
                                    , created_by_step :: forall a.Step a
                                    }

The C++ machine is untyped, so in some sense the type for `used_force_regs`
might be `Vector Node`.

The current machine has a mapping from `Int` to each cell, but this is
basically a pointer.

This doesn't quite say how we would connect `exp :: Expr a` to other force-reg cells.
I guess `Expr a` would be an expression in a "runtime" AST...

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

Now that we have `List@C a = Changeable (List#C a)`, it is no longer awkward to
have

    read MODE x as x' in
    case x' of z alts       -- with possible case binder `z`

Now `x'` would have type `List#C a`.
The earlier fused read/case notation is unnecessary because the generated
representation type can express this one-layer view directly.

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
      read USE z as z' in
      case z' of
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
        read USE z as z' in
        case z' of
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
reads.  Without such a scope, `read USE z` specifies the edge target but not
the cell that owns the edge.  The exact normalized form still needs design.

## Merged Continuations

Some useful programs need dependencies whose target is discovered while
evaluating a protected reduction.  For example:

    lookupMaybe a i =
      noreplace {
        read USE i as i'
        case a ! i' of tmp ->
          merged {
            read USE tmp as tmp' in
            case tmp' of
              Just x  -> Just x
              Nothing -> Nothing
          }
      }

Here `tmp` is computed from `a ! i'`.  If `tmp` is not materialized as a heap
cell, then `read USE tmp` has no stable heap target.  The current machine would
need let-floating or allocation so that `tmp` names a heap cell.

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
`Int`.  A `read` on `xs` exposes one `List#C` constructor layer; in the cons
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
