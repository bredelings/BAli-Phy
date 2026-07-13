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


## Other latent site and letter properties

The properties of interest here should be latent features of the generative
evolutionary model, rather than statistics computed directly from the observed
alignment.  For example, whether a site is under positive selection is a latent
model property, whereas the entropy of the observed alignment column is not.
Likewise, charge and polarity of an observed amino acid are normally directly
observable at a tip and are not latent properties.

It is useful to distinguish two kinds of latent information:

* **Process properties** are parameters or regime assignments in the
  substitution model, such as a rate class, a dN/dS class, a state-fitness
  profile, or a hidden Markov-modulation state.

* **History properties** describe an unobserved evolutionary history, such as
  ancestral states, substitution counts, time spent in a regime, or changes
  between hidden regimes.

Both kinds are inferred under the generative model, although history properties
usually need to be marginalized over possible substitution histories.


### Posterior expected substitution counts

Posterior expected counts can be summed over every branch to produce one scalar
for each site.  For site `s` and event class `C`, define

```text
E[N(s,C) | data] = sum over branches b of E[N(s,b,C) | data].
```

The result may be fractional because it is a posterior expectation rather than
a count from one sampled history.  Stochastic substitution mapping and labeled
continuous-time Markov-chain rewards provide standard ways to calculate these
expectations without choosing a single reconstructed history.  See
[Nielsen's mutation-mapping paper][nielsen-mapping] and
[Minin and Suchard on labeled transition counts and dwell times][minin-suchard].

Potential tree-aggregated site properties include:

| Property | Interpretation |
| --- | --- |
| `E[N(all)]` | Expected total number of substitutions |
| `E[N(C)]` | Expected number of substitutions in class `C` |
| `P(N(C) > 0)` | Probability that at least one event in class `C` occurred somewhere on the tree |
| `E[N(C)] / E[N(all)]` | Composition of substitutions, conditional on there being changes |
| `E[T(C)] / E[T]` | Expected fraction of evolutionary time spent in latent regime `C` |
| `E[N(switch)]` | Expected number of changes between hidden regimes |

Event classes could include:

* transitions and transversions;
* synonymous and nonsynonymous substitutions;
* beneficial, nearly neutral, and deleterious substitutions under a
  mutation-selection model;
* preferred-to-unpreferred and unpreferred-to-preferred changes;
* changes between profile or Markov-modulation states; and
* compensatory and disruptive changes under a paired RNA model.

Raw expected counts grow with total tree length.  That is generally harmless
when comparing sites on the same tree, but comparisons between datasets, or
between sites with substantially different missing-data coverage, may require
normalization by usable branch length or another measure of exposure.
Synonymous and nonsynonymous counts also cannot by themselves measure selection:
their different opportunities must be accounted for, as in dN/dS.

`P(N(C) > 0)` may sometimes be easier to display than an expected count.  It is
bounded between zero and one and is less dominated by multiple events on a few
long branches.  In contrast, `E[N(C)] / E[N(all)]` describes the composition of
changes but may be unstable at sites where very little change is expected.


### Site-and-letter fitness properties

A mutation-selection model can infer a relative fitness or preference `F(s,a)`
for every possible letter `a` at site `s`.  This is a genuine site-and-letter
property even though charge or polarity of the observed tip letter is not.  The
selection coefficient for a possible change can then be represented by

```text
S(s, a -> b) = F(s,b) - F(s,a).
```

Derived site properties could include the posterior fractions of possible
mutations that are beneficial, nearly neutral, or deleterious, or the strength
of preference for the optimal state.  Only relative fitnesses are identifiable,
so they should be reported relative to a reference state or through inferred
stationary preferences.  Furthermore, an arbitrary equilibrium-frequency
profile should not be called a fitness profile unless the mutation-selection
model supplies the relationship between mutation, fixation, and equilibrium
frequency.  Sitewise mutation-selection fitness models are discussed by
[Rodrigue et al.][rodrigue-mutsel] and [Tamuri et al.][tamuri-selection].

The concentration of an inferred fitness or preference profile can describe
how many states the model says a site tolerates.  Although this could be
calculated using entropy, it should be called something like **selective
breadth** to distinguish it from entropy calculated from the observed alignment
column.


### Other process properties

Other potentially useful latent properties include:

* **Differential selection or fitness-landscape shifts.**  A model with
  foreground/background, environmental, or clade-specific profiles can report
  the posterior magnitude of the profile shift and which letters became more or
  less favored.  [Parto and Lartillot][parto-lartillot] give an example of
  condition-specific amino-acid preferences.

* **Latent rate-state occupancy.**  For a covarion or Markov-modulated model,
  useful site summaries include the expected fraction of tree time spent in
  each state, the probability of being in a state at a selected node, and the
  expected number of hidden-state switches.  Covarion states can be treated as
  latent histories that switch along the tree, as in
  [this treatment of heterotachy][heterotachy].

* **Mixture-component membership.**  Posterior probabilities over profile or
  process components are latent site properties.  Numeric component labels may
  be scientifically meaningless and can suffer from label switching, so an
  averaged profile or a derived biological quantity is generally preferable.

* **Nonstationary compositional regimes.**  If the model contains distinct
  mutational or selective regimes, it could report their posterior occupancy
  and the magnitude of changes such as an AT-to-GC bias shift.

* **RNA structural regimes.**  Paired/unpaired status, pairing partners, and
  compensatory histories are latent properties only when the evolutionary model
  infers them.  If secondary structure is supplied as fixed annotation, they do
  not belong in this category.

* **Codon-usage or synonymous-selection regimes.**  These qualify when codon
  fitnesses or synonymous-rate classes are inferred by the model, rather than
  calculated directly from observed codon usage.

* **Alignment and indel-process properties.**  Since BAli-Phy models alignment
  evolution, expected insertion/deletion counts, indel-rate classes, or
  posterior occupancy of conserved and indel-prone regimes may also be useful
  latent site properties.


### Episodic selection and branch-specific properties

Episodic selection is naturally a site-by-branch property, but the complete
array does not need to be shown alongside every sequence.  Useful site-level
summaries include:

* the posterior probability that positive selection occurred on at least one
  branch;
* the expected fraction of total branch length spent in a positive-selection
  regime;
* the expected number of nonsynonymous substitutions generated in that regime;
  and
* site-level posterior evidence for admitting an episodically selected branch
  class.

MEME similarly obtains site-level evidence by marginalizing over which branches
carry an episodic class; individual branch assignments require more caution.
See the [MEME paper][meme].

A display could expose branch-specific information at three levels:

1. Show a single tree-aggregated value for every alignment site.
2. Optionally show foreground/background or inside/outside-clade summaries.
3. When the user selects a site, color the tree with the corresponding
   per-branch posterior values.

This keeps the normal alignment display manageable without discarding the
underlying branch information.

Convergence is harder because it relates pairs of branches.  A site can still
be summarized by the probability of at least one convergent event or by the
expected number of convergent branch pairs, but the latter needs normalization
because the number of possible pairs grows rapidly with the tree.  When lineage
or environmental regimes are modeled explicitly, an inferred convergent shift
in fitness profiles may be more meaningful than merely counting repeated
states.


### Initial priorities

The most generally useful additions appear to be:

1. Tree-summed posterior labeled substitution counts.
2. Site-and-letter fitness or preference profiles.
3. Hidden-regime occupancy and switch counts.
4. Differential-selection summaries.
5. Per-branch values retained for optional tree drill-down rather than treated
   as ordinary alignment-column properties.

[nielsen-mapping]: https://academic.oup.com/sysbio/article-pdf/51/5/729/19502899/51-5-729.pdf
[minin-suchard]: https://vnminin.github.io/papers/Minin2008a.pdf
[rodrigue-mutsel]: https://pmc.ncbi.nlm.nih.gov/articles/PMC2842053/
[tamuri-selection]: https://pmc.ncbi.nlm.nih.gov/articles/PMC3296245/
[parto-lartillot]: https://pubmed.ncbi.nlm.nih.gov/28645318/
[heterotachy]: https://pmc.ncbi.nlm.nih.gov/articles/PMC2607421/
[meme]: https://pmc.ncbi.nlm.nih.gov/articles/PMC3395634/
