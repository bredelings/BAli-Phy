# Component-State properties


We now have infrastructure to output property values for each (component,state)
pair.  One of the main unsolved problems is how to automatically generate
the component-state information.

One approach is put the the map from names to property values into the model
object.
This would prevent us having to implement something separate in the JSON files.

We could put the map from Text -> Property values on each component.  This
would handle things like:

    |w: gtr +> x3 +> Rates.dNdS(w)| +> m3(3)
    
The idea is that each individual component would have a mapping from `"dNdS"` to
`Constant(w[i])`.
If there is just one component, this is not "interesting", and ideally we would
not create any properties.
Instead, of creating properties, we create a logger for `dNdS = w[0]`.
However, if there are multiple components, then we should

 * log w1, w2, ... w[n]
 
 * write out the w property values for each component.

However, if the number of components is variable, the not writing properties for
"uninteresting" properties might not work.

Another idea would be that the m3 model annotates the components with "dNdS"
and "posSelection", and that dNdS does not.


## Complexities

The simplest case is that we have C components, and each component has a single rate matrix.
Then the property is a function of the rate matrix for component C.
If the property is the rate for component C, then we can calculate it from the Q matrix
  for each component.
  
### Non-computable properties

If we also consider dN/dS properties, then we **cannot** compute them from the Q matrix.
These properties depend on the ratio between the final matrix and the neutral matrix,
and we no longer have the neutral matrix.

### Markov-modulated models

Markov-modulated models take a collection of Q matrices Q[i], and allow
switching between them via a switching matrix S.  Each state k comes from some
original (i,s) where i specifies the Q matrix, and s specifies the state within
it.

In this case, we can find out the state (i,s) at the tip of each branch.
The property for each tip can be reconstructed from the property state s in
matrix Q[i].

Reporting the properties at the leaves fails to capture properties at parts of
the tree in between the leaves.

### Different Q matrices on each branch

Here again a full answer to what is happening at each site requires information
about what happens over the entire tree.

However, the obvious thing to do is to report the state at the leaves.
If each component can have multiple Q matrices then specifying the component
isn't sufficient to specify the rate regime.
Instead, we need to know not just the component at each tip, but the Q matrix at
each tip.
Instead of specifying (component,state) info, we need to specify (matrix,state)
info.

### Modification of Q matrices

When we do `gtr + x3 + dNdS + fMutSel`, the positive selection info is possible
to retain.
However, if might be safer to retain the positive selection info only if we do
  `gtr + x3 + fMutSel + dNdS`.
This raises the questions of what kind of transformations we want to simply drop
  any properties, and which ones we want to allow to retain properties.
Its possible that we really only want the properties to be retained under pure
  scaling transformations.




## Case: rates

How do we handle the case of "rates"?

 * one way is to give many models of a property of "rate" -> Constant(1)
 
 * alternatively, we would add rate information only in side the ASRV.Gamma model.

## TODO

 * Can we currently "compress" cases where every letter maps to the same value?
   Do we need to?

 * Add a map from Text -> Properties to each SModel.Markov model.  

 * Make Rates.gamma create a "rate" property for each component.

 * Make dNdS create a "dNdS" property for each component

 * Make dNdS create a "posSelection" property

 * Make markov-modulation create letter properties for each component.

## Complications

 * What if each branch has a different Q matrix?
 
   I think in this case, we want each leaf to use the Q matrix
   assigned to its leaf branch.
   
 * What if the matrix is later rescaled?
 
   dNdS and posSelection properties are unchanged.
   
   rate properties are scaled by the rate.
   
 * What if the matrix is modified by something that is NOT a rate?
 
   Probably we should just drop all properties.
   
 * What if we create 
