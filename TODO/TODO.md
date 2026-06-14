# TODOs

## Infrastructure

 * Complete native windows tests.
 
   - Possibly we need to do something to make bp-analyze work.

## Runtime refactor

 * Put Trim into the AST
   * This would be simpler if we make a deindexify\_and\_trim.  Which _should_ be possible.

## Command-line UI / bindings

 * infer haskell types for bindings

## Structural Problems

 * stop using the C++ stack, so that we can have unlimited stack growth.
 
 * implement ST so that we can actually code stuff that modified memory, such as creating vector<double>.

   - we want to RECORD all uses/forces.
   
   - however we want to especifically avoid making any individual reduction something that can be redone.

 * JIT compilation 

   - for code that either only records USES, we shouldn't need to return a result each time

   - for code that also has no changeable nodes, things should be even simpler.
   
   - for code that records USES and also does changeable reductions, maybe we can still directly execute some stuff
     instead of doing switch statements.
     
   - if each reg has only one interpreter (which requires merging incr_eval_1 / incr_eval_2) then JIT compilation would make more sense.

## Speed

 * Investigate using heapcheck

 * Replace Runtime::Exp with a hand-written discriminated union?

 * Delete the constructors that make an Object using clone?
 
 * Create data families so that EVector Double can be backed by std::vector<double>

   - also maybe, EPair Double Double

   - also EIntMap Int -- could implement a strict IntMap
   
 * could I / should I introduce an actual ByteArray# for unboxed things with no destructures like Vector Double


## Haskell correctness & completeness

 * audit and improve RecursiveDo support
 
 * audit and improve higher-rank support
 
 * audit and improve kind checking support
 
 * allow forall in inferred kinds
   - allow user-written foralls
   - local tycon generalization -- only when infering tycon kinds.

 * PolyKinds (i.e. forall k in kinds)
 
 * DataKinds (i.e. using Bool as a Kind and 'True as a type)
 
 * Implement Data.Vector
 
 * Implement Data.Array correctly
 
 * Clean up error message printing

   - record all the types so that we can jointly tidy them using an implicit tidy state, similar to constraint warnings
   - use a text algebra similar to Wadlers.
   - handle colors in the text algebra -- otherwise after we finish green text we don't know what to go back to.
   
 * downloading packages from hackage?
 
 * unboxed tuples?
 
   - how do operations return multiple things?
