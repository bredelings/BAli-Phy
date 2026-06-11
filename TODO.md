# TODOs

## Runtime refactor

 * Put Trim into the AST
   * This would be simpler if we make a deindexify\_and\_trim.  Which _should_ be possible.

 * Replace shared_ptr in ObjectValue -- this makes it slower?

 * Split App into FuncApp, ConApp, OpApp.
   - Are we allowed to return ConApp from e_op?  I think we already do for BranchAlignment / NodeAlignment.

## Refactor

 * infer haskell types for bindings

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

 * record syntax -- still need record wildcards and several other rhings?
 
 * PolyKinds (i.e. forall k in kinds)
 
 * DataKinds (i.e. using Bool as a Kind and 'True as a type)
 
 * Implement Data.Vector
 
 * Implement Data.Array correctly
 
 * downloading packages from hackage?
 
