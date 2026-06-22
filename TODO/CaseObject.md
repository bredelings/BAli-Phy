# Case objects

We'd like to evaluate case objects directly because:

 * doing an allocation for each case object is too many evaluations

 * directly evaluation of the objects separates expression evaluation
   from reg evaluation.

## How to do generic evaluation

If we are going to do case E of alts without allocating a new reg for E then
we need to have a routine that does evaluate(E.exp, E.env) and records the 
uses on the current step.

By passing the exp and the env separately, we allow them to be decoupled.
Initially we would pass Reg.exp.object and Reg.env.

## What if the object evaluates to an index-var

The two strategies are:

* create a new expression where the object is replaced by an index-var and call
  that.
  
* modify steps to track dependent regs

## Normalization

In order to do this we need to stop replacing case objects by reg-vars during
normalization.
Its possible that we could do this in an initial, separate phase by moving
let-allocating into the executing of the case op.

