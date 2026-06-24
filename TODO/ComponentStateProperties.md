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
