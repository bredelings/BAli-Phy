# Recording Use/Force on Steps

Right now we record all USE and FORCE edges on regs.
This makes sense for operations like (x+y), where the inputs are fixed.
However, there are a few cases where it causes some pain:

 * case (f x) of alts
 
   In this case, we want to record a fixed USE of `f` and `x`, if they are
   changeable.  However, if `f x` result a RegRef or IndexVar, then we want
   to record a dynamic / contigent USE on that reg.
   
 * runST f g s
 
   We could in theory manually implement
   
        case (f s) of (s2,y) ->
          case (g y) of ST h ->
            h s2
            
   By implementing this manually, we could force (f s) and (h s2) to happen
   in a single step.

The `case (f x) of alts` example shows that allowing contingent USE and FORCE
on steps is equivalent to a CALL edge, in that it indicates dependent execution.
It allows implementing `case object of alts` without allocating a fresh cell
for the object, whereas right now we need the fresh cell in order to obtain a
call edge, which is the _only_ way that dependent execution can be represented.

## Dependent execution

Allowing new ways of expressing dependent execution may mean that we need to
update how `unshare_regs2(t)` in `reroot.cc` handles dependent execution.

Right now `increment_counts_from_new_calls` walks all changed steps and
increments the increments the force_count for regs at the end of call edges
from regs with force_count > 0.  (QUESTION: if we walk this list a second time,
will be find more edges to increment?)  We would need to increment edges for
dependent USE/FORCE as well.

## Representation

One possibility would be to make a `small_vector<edge>` edges that records 
CALL and USE and FORCE edges, to replace the current `call_edge` member.
The `edge` object is currently `tuple<int,int,int>` and records:

 * the called reg R1, which might be an index-var.
 
 * the reg R2 at the end of the index-var chain, IF CHANGEABLE, and -1 otherwise.
 
 * the in position regs[R2].called_by corresponding to the current call, if R2 is changeable.
 
The called\_by edge is only needed if there is a call to something changeable.
But we still need to record the called reg_ref, because it might be a reg_ref_with_force.

## Related cleanups

 * Create a central reg store vector<Edge> where an Edge contains
 
        struct Edge
        {
            int from;
            int to;
            int from_index
            int to_index;
            char type; // USE, FORCE, CALL  -- or use an enum class
            char from_type; // REG, STEP    -- or use an enum class
        };

 * Possibly added created_reg CREATED_REG also?  Alternatively, we could have a different central store for CREATED_REG edges.


