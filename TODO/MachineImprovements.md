# Machine Improvements

## DONE: Combine force/uses into one list

## DONE: Dependent uses/forces

## Merge incremental_evaluate{1,2} to be one function?

Main differences:
 
 * `force_regs_check_same_inputs`.
 * ??

Should flags be in a register or function argument?
How much slower would this make stuff?

## Separate created-by-reg and created-by-step

Issues:

* For non-contingent regs, we should be able to drop the edge.

* reg_exists(r) currently checks if the creator step is mapped in the root.
  So we might need the transitive creator step even if we record the creator
  reg.
  
* unshare_regs1( ) current unmaps regs created by unmapped steps.
  So we might need to walk the transitive creator graph.

## Speeding up the interpreter

### Inline Array.!, Int.+, etc, maybe case, let, etc?

Can we avoid function calls for common/fast cases?

## Implementing our own stack

This is necessary to not crash for programs that walk medium-sized or larger
data structures.

We need to start by converting non-operation-args related recursion to loops.

 * probably merge incremental_evaluate1/2
 * Change incremental_evaluate1/2( ) into a loop with just Enter/Return as on `stack-refactor2`
 * inline incremental_evaluate1/2_ into the only caller
 * maybe change helper contracts to stop returning `result`, but instead run a eval_postlude 
   that pops the stack and puts the result into the parent frame's mailbox


 * modify builtins to push the work of evaluating their arguments onto the
   stack, followed by a "finalizer"
 * modify evaluation to have a kind of loop where we continually pop the 
   top operation off the stack and execute it.

See [Stack](Stack.md).

Does this really need all the other stuff done first?  Maybe not.

### Case evaluation and strict arguments

Preparing builtin arguments with `case` may simplify evaluator frames, but it
also changes graph allocation, dependency boundaries, and reuse.  The runtime
representation is discussed in [CaseEvaluation](CaseEvaluation.md), while
demand signatures and optimizer transformations are discussed in
[StrictArguments](StrictArguments.md).

## Don't allocate case objects

Whether eliminating a case register saves work or removes a useful cache node
depends on invalidation and reuse.  See [CaseEvaluation](CaseEvaluation.md).

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

## Replace e-ops with something more general

General strict-argument preparation could subsume e-op argument handling, but
case conversion has different caching costs in the incremental graph.  See
[StrictArguments](StrictArguments.md) and [CaseEvaluation](CaseEvaluation.md).


## Use global forward/backward edges

Instead of storing the target + the index of the back-edge, we store
a global reference, which includes forward, back, forward_index, back_index,
and maybe some other stuff.

This allows a uniform implementation, but maybe some worse cache-locality, as
we need to look at forward + backward + global list, instead of just 
forward + backward.

Q: Maybe this is actually bad?  We change [x,i] <-> [y,j] to

    i:k <-> [k:x,y,i,j] <-> j:k
    
So if we delete i, we need to

 * copy the last element k2 over k, then update k2.i and k2.j to point from k2->k
 * copy the last element j2 over j, then update the combined entry to point from j2->j
 * copy the last element i2 over i, then update the combined entry to point from i2->i
 
So there is 50% more work. 
