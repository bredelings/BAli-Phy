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

The main wrinkle is that if we USE / FORCE a cell that has **not** been
allocated by this interpreter, then we want to revert to the interpreter that
allocated the cell.  Does that means that we would need to record the
interpreter on each reg?  See [Marking each cell with its interpreter](Interpreters.md).

## Can runST cells leak out of a runST evaluation?

In theory, the type system should guarantee that no cells marked with the runST
interpreter should leak out of the evaluation of a runST calculation.

However, what the type system actually guarantees is that nothing will leak out
that mentions the state type `s`.  Suppose we have a calculation of type
`ST [a]`.  It seems that probably the nodes for the `[a]` would be evaluated
under the runST interpreter, and they WOULD leak out.

Hmm.. perhaps we mainly want just the `(>>=)` operation to be run under the
special interpreter?  If we have something like

    makeString x y = do
                       s <- allocateEmptyStringST
                       writeStringST s x
                       writeStringST s y
                       return s

then we want the three operations ending in ST to be glued to gether.

However, if we have something like 

    makeList x = do
                   let s = (x:x+1:x+2:[])
                   return s

then we do not want the allocated list cells to be tagged with the runST
interpreter.

However, if we have

    instance Monad (ST s) where
        {-# INLINE (>>=)  #-}
        (>>) = (*>)
        (ST m) >>= k
          = ST (\ s ->
            case (m s) of { (# new_s, r #) ->
            case (k r) of { ST k2 ->
            (k2 new_s) }})

then I think we want the `m s`, the `k2 new_s`, and possibly the `k r` to be
evaluated using the runST interpreter, _even if they are let-allocated_.

### How it used to work

Previously, I think we use a `f \`join\` g` function that used f and evaluated to g
The idea was that if either f or g is invalidated, then (join f g) is invalidated.
We used this to implement `>>=` so that if any `>>=` in the whole computation was
invalidated, then the top-level one was as well.

This may have been combined with `reapply f x`, which allocates `f x` and also uses it,
so that if `f x` is invalidated, we allocate a new `f x` and evaluate it from scratch.
The idea might be that if we do `reapply runST x`, then if the top-level `runST x` gets
invalidated, then the `reapply` is also invalidated, and we make a new `runST x` node
and so the computation is rerun from scratch.

That seems to validate the idea that only the `>>=` operation needs to be treated
specially.

### Current state

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

GHC has 

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

