# Machine Improvements

## Implementing our own stack

This is necessary to not crash for programs that walk medium-sized or larger
data structures.

We need to 

 * modify builtins to push the work of evaluating their arguments onto the
   stack, followed by a "finalizer"
 * modify evaluation to have a kind of loop where we continually pop the 
   top operation off the stack and execute it.

See [Stack](Stack.md).

### All evaluation done with case?

If we could require that all evaluation is done with case, then we might
be able to have a lot fewer frame types.
Hoever, we might want to translate current builtins more mechanically, so
that each evaluation member function of args corresponds to a different
argument evaluation frame type.

We could modify builtins to push evaluation frames for their arguments.
I wonder if we could require that all arguments are already evaluated
 via case, so that a builtin ins something like
 
    case x of _ -> case y of _ -> builtinOp x y
 
The problem is that we want to record a USE on the builtinOp, but the above
would (i) record a force instead of a use and (ii) would record it on the
case.  Possibly we could switch to something like:

    case x of x' -> case y of y' -> builtinOp x' y'

Possibly in this case we somehow delay the USE until something consumes the
 x'?  I'm not quite sure what the framework would be here.
GHC adds a binder separately of the pattern, which would be _ in this case,
so

    case x of x' _ -> case y of y' _ -> builtinOp x' y'
 
So maybe if the binder is not dead, then we record a USE and keep on going? 
Presumably if `x` is a reg, the `x'` is then same reg?

Possibly `x'` and `y'` would go either (i) in the closure or (ii) on the stack?
Right now I assume that only pointers can go on closures.  But in theory I could
try and make Core variables have kind LiftedPtr or Unlifted {Double, Int, Char, LogDouble_t, or ObjectPtr}.
If that could survive optimization, then we could use it in code-generation and put
it in the runtime.  I wonder if we could arrange that all the LiftedPtr references come
first, so that it would be easy to loop over them.  Possibly we would reference closure
entries by their byte offset into the closure environment?

In any case, it seems easier to put unlifted values on the stack that in a closure.

## Don't allocate case objects

`case (op E1 E2 E3)` of alts should not allocate a separate reg for the op,
although it _should_ allocate separate regs if E1, E2, and E3 are separate
expressions.

If E1 .. E3 are "cheap" expressions, though, then perhaps we shouldn't allocate
them either, since 
 * we do have the cost of recomputing E2 and E3 if E1 changes.
 * but if this is cheap enough, then caching E1/E2/E3 separately is slower than
   recomputing them from scratch.

See [CaseObject](CaseObject.md).

If E1 .. E3 are replaced by x .. z, then we still need a stack to force the
evaluation of x .. z.  However, if we can incorporate them into the case object
evaluation, then ideally we use a stack that can evaluate sub-expression
without switching to a new reg.

## Dependent uses/forces

We would need to store these on the step.

## runST

We need to make 

    case f s of (s2, x) -> case g x of ST h -> h s2
    
run without let-allocating anything.  That ensures that it is 
impossible to cache the first part (i.e. `f s`) separately from 
the second part `h s2`.

This may 
* require the dependent uses/forces above.
* benefit from directly evaluting case objects w/o allocation.

See [runST](runST.md).

## Combine force/uses into one list

If we put uses/forces on steps, then perhaps we would
also put the call in the same list.

## Separate created-by-reg and created-by-step

One issue is what to do with non-contingent regs.

## Allocate non-escaping regs on the stack

Some let-allocated variables are marked as going on the stack.
Currently we do this with e-ops, but this is more general.
We might need to implement a stack for this to work.

## Use global forward/backward edges

Instead of storing the target + the index of the back-edge, we store
a global reference, which includes forward, back, forward_index, back_index,
and maybe some other stuff.

This allows a uniform implementation, but maybe some worse cache-locality, as
we need to look at forward + backward + global list, instead of just 
forward + backward.
