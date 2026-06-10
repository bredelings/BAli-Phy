# TODOs

## Runtime refactor

* Put Trim into the AST
  * This would be simpler if we make a deindexify\_and\_trim.  Which _should_ be possible.

* Replace shared_ptr in ObjectValue

* Split App into FuncApp, ConApp, OpApp.
  - Are we allowed to return ConApp from e_op?  I think we already do for BranchAlignment / NodeAlignment.

## Speed

 * How to handle the operator-constructing wrapper inside the evaluator?
 
   * This could become simpler if we split App into FuncApp / ConApp, OpApp above
   
   * Maybe a single hand-coded switch statement?

 * Replace Runtime::Exp with a hand-written discriminated union

 * Delete the constructors that make an Object using clone?
 
 * Create data families so that EVector Double can be backed by std::vector<double>

   - also maybe, EPair Double Double

   - also EIntMap Int -- could implement a strict IntMap


## Correctness/Completeness

 * properly support newtype
 
 * record syntax
 
 * deriving Eq, Ord, ... ?

 * downloading packages from hackage?
