# Marking each cell with its interpreter

The idea is that when allocating a cell under a specific interpreter, we mark
the cell as being associated with the interpreter that allocated it.  Then when
we go to evaluate that cell, we use the interpreter that allocated it regardless
of which interpreter is forcing the evaluation.

This has the benefit of ensuring that cells evaluated under the unchangeable
interpreter would only ever be evaluated by that interpreter.  Which means that
we could perform optimizations on them that wouldn't be allowed under the regular
interpreter.

## JIT compilation
It also means that we could JIT-compile each cell to code that runs inder the
specific interpreter!

## Switching interpreters
If we want to switch interpreters, then we need to allocate a new cell, mark it
as having a different interpreter, and evaluate it.

## Copying results from one interpreter to another?
Currently, we are assuming that converting evaluation results from one
interpreter to another interpreter is a no-op.  And currently this is actually
true.

However, if we represent cells under the changeable interpreter as something
like

    data ChangableCell a = Unchangeable a | Changeable a
    
then in theory we could (i) represent more directly what the changeable
interpreter is doing at the Core level and (ii) we would need to translate from
the result in the changeable interpreter to a result in the unchangeable
interpeter.

## Wrinkle: incremental_evalaute1 vs incremental_evaluate2

The main wrinkle here is that for changeable cells we sometimes want to do
`incremental_evaluate1`, and sometimes want to do `incremental_evaluate2`.
That is, we sometimes want to evaluate changeable cells (i) without performing
their effects and (ii) without updating force_counts. (Is that the only
difference?)

