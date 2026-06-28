# Category-State properties

We would like to print out the posterior mean of certain properties for each
letter in each each sequence.
When the property depends only on which mixture component (i.e. category) a 
state is in, then the property is just a category property.
The rate category under a GTR+G+I model would be an example of this.
However, when we add Markov-modulated models, then what were previously
category-specific properties become letter-dependent properties.

## Logging format
We write out the letter and category of each letter in each iteration.
We also write out the property value for each letter and category.
This allows us to compute the posterior mean property value for each
letter, category, and property.


## Infrastructure
We currently have an example program that generates category-state samples
for GTR+I+G in in `~/Sync/Main3.hs`.
The properties map is manually constructed instead of being coming along with
the model:

    let result = gtr' sym pi alpha +> unitMixture +> SModel.gammaRates alpha_2 4 +> plusInv p_inv
    let loggers = ["gtr:sym" %=% sym, "gtr:pi" %=% pi, "Rates.gamma:alpha" %=% alpha_2, "inv:p_inv" %=% p_inv]
    let properties = M.fromList [("rate", rateProperty $ scaleTo 1 result)]
    return (result, loggers, properties)

The `properties` variable has the type 

    Map String (ComponentStateProperties Double)
    
## Can we compute the properties from the model?

One of the main unsolved problems is how to automatically generate the 
properties map.
The properties map has the conceptual type:

    PropertyName -> Category -> State -> value

Its not clear which property names should be in the map for each model.
Its not clear how to generate the map for the relevant property names.

### User-specified properties or automagically generated properties?

One approach would modify construction of the model to automatically construct
names and relevant values.

An alternative approach would require the user to construct the properties map.

### Can we compute the property values from the completed model?

For GTR+G+I the properties depend only on the category, and not the letter.
We can then compute the property for each category from the corresponding rate
matrix for that category.
This is what the example above does.

However, for covarion models (e.g. GTR+G4+I+Galtier01) the rate property now
depends on the letter.
While GTR+G4+I has 5 categories, GTR+G4+I+Galtier01 has only 1 category.
The 5 different categories have now been combined into one rate matrix with 5
times as many states.
The rate for each state (l,k) for original letter l and original category k
should presumably be the rate for category k in GTR+G4+I.

For dN/dS properties, the property is a ratio of the current rates
to what the rate would have been before we added the dNdS property.


## 

###


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

## Existing infrastructure



## Case: rates

How do we handle the case of "rates"?

One way is to give many models of a property of "rate" -> Constant(1).
But then how do we handle the operation of scaling the rate?
* Would we look for a property called "rate" and scale it?
* Would we make properties a function of the rate matrix?

Alternatively, we would add rate information only in side the ASRV.Gamma model.
But then how does plusInv work?
It needs to add a rate=0 property annotation to the new category.
Does it also need to scale the rates for all the other categories?

Triplet models could have different rates for the 1st, 2nd, and 3rd position,
as well as an overall rate.
So that would be 4 rates.

Codon models additionally have an amino-acid rate, leading to 5 rates.
Additionally, we may be interested the synonymous and non-synonymous version
of all of those rates, leading to a total of 10 rates.
Basically, that would be a matter of the rates before/after the dNdS operation.

We could also consider the rates before/after a fMutSel operation.


## TODO

 * Can we currently "compress" cases where every letter maps to the same value?
   Perhaps, (Constant 1) versus (ForEachLetter [1, 1, 1, 1])?
   Do we need to?
   Perhaps this is extra complexity we should ignore to being with.

 * Make dNdS create a "dNdS" property for each component

 * Make dNdS create a "posSelection" property

 * Make markov-modulation create letter properties for each component.
