# Implementing runST inside the machine

So, the main idea is that (runST action) should 

 * record USE and FORCE during the ST action
 
 * should require the entire action to be invalidated jointly.
 
For example, if the ST action allocates a buffer, then writes a changeable
value to the buffer, then we do not want to rerun only the write operation.
Even though the allocation has no changed inputs, we must rerun it also.
Otherwise, we will modify the result of a previous operation, in a different
context, instead of creating a value result for this specific context.

Any cells that are let-allocated during an ST operation should be reachable
only from within that operation.  If we allocate a new cell to run the operation
and give it its own step, then all the cells that are let-allocated from within
the ST operation will show that step as their creator.  When it is destroyed,
they will also be destroyed.  As a result, any evaluation during these cells
can safely be run using a different interpreter, if that is helpful.

So the idea is that runST action, allocates a new cell containing something like

    runST# intialState action
    
and then turns into an index-var pointing to that new cell.  The cell itself 
allocates a step, and all USES/FORCES are recorded on that single step.
That step should then CALL the result if any changes are recorded.  If no
changes are recorded then will be replaced by the result.

## Using a different interpreter

In changeable evaluation, we normally want to avoid replacing the current
closure with its one-step-reduction result.  Instead we allocate a new cell
and allocate the result in the new cell, recording a CALL edge from the current
cell to the new cell.  This allows us to replay the execution with different
inputs if the inputs change.

However, inside the ST interpreter, we specifically do NOT want to do that.
We want to accumulate any dependencies, but if any of the inputs change, we do
not want to be able to replay part of the combined action.

This suggests a different interpreter that always replaces the expression with
its result, and never uses CALL edges.

If, however, we USE / FORCE a cell that has not been allocated by this
interpreter, then we want to revert to the interpreter that allocated the cell.

### Recording the interpreter on cells are allocated under it?

I suppose if we start running a program under the unchangeable interpreter, then 
also any cells that were allocated by the unchangeable interpreter should be
allocated by that interpreter as well.

If a cell is only evaluated by the interpreter that allocated it (with escape
hatches to allow switching interpreters by allocating a new cell and marking it
as having a different interpreter), then this means that we could actually JIT
compile each cell to contain code for "run operation X under interpreter Y".

The main wrinkle here is that for changeable cells we sometimes want to do
`incremental_evaluate1`, and sometimes want to do `incremental_evaluate2`.
That is, we sometimes want to evaluate changeable cells (i) without performing
their effects and (ii) without updating force_counts. (Is that the only
difference?)

### How it would work

Right now we have

    data ST s a = ST { runST :: s -> (s, a) }

    instance Monad (ST s) where
        f >>= g  = ST (\state1 -> let (state2,x) = runST f state1 in x `seq` runST (g x) state2)
        unsafeInterleaveIO f = ST (\state -> (state, snd $ runST f state))

    instance Monad (ST s) where
        f >>= g  = ST (\state1 -> 
                   case runST f state1 of (state2,x) -> 
                   x `seq` runST (g x) state2)
        unsafeInterleaveIO f = ST (\state -> (state, snd $ runST f state))

I guess GHC has 

    -- @'runST' (writeSTRef _|_ v >>= f) = _|_@
    newtype ST s a = ST (STRep s a)
    type STRep s a = State# s -> (# State# s, a #)

    instance Monad (ST s) where
        {-# INLINE (>>=)  #-}
        (>>) = (*>)
        (ST m) >>= k
          = ST (\ s ->
            case (m s) of { (# new_s, r #) ->
            case (k r) of { ST k2 ->
            (k2 new_s) }})

### How it used to work

Previously, I think we use a `f \`join\` g` function that used f and evaluated to g

 - performed and used g
 - performed and used h

The idea was that if either f or g is invalidated, then (join f g) is invalidated.

This may have been combined with `reapply f x`, which allocates `f x` and also uses it,
so that if `f x` is invalidated, we allocate a new `f x` and evaluate it from scratch.
