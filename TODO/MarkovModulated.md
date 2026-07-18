# Markov-modulated models

This note records the problem with the current Markov-modulated model and the alternatives considered for
supporting component rate matrices with different equilibrium frequencies.

## Definitions

Let `i` and `j` identify hidden regimes and let `x` identify an observable character state.

* `Q_i` is the character rate matrix used while the process is in regime `i`.
* `pi_i[x]` is the stationary frequency of `x` under `Q_i`.
* `M[i,j]` is the proposed mean rate of switching from regime `i` to regime `j`.
* `alpha[i]` is the stationary frequency of regime `i` under `M`, so `alpha M = 0`.
* `mu(i,x)` is the desired stationary frequency of the expanded state `(i,x)`.

The natural product-form target is

    mu(i,x) = alpha[i] pi_i[x].

It says both that the regime frequencies are `alpha` and that the conditional character frequencies within regime
`i` are `pi_i`.

## Current construction

The usual state-retaining, autonomous-switching construction has rates

    (i,x) -> (i,y) at Q_i[x,y]
    (i,x) -> (j,x) at M[i,j].

This is the construction in `src/builtins/SModel.cc`.  The code also reports `mu(i,x)` as the equilibrium and may
mark the expanded model reversible.

When the `pi_i` differ, however, the switching part generally does not preserve `mu`.  At `(i,x)`, stationarity
would require

    sum_j alpha[j] pi_j[x] M[j,i]
        = alpha[i] pi_i[x] sum_j M[i,j].

Stationarity of `alpha` under `M` cannot cancel the different `pi_j[x]` factors.  For an irreducible autonomous
switching process, product form normally requires all component matrices to have the same stationary distribution.
Using the incorrect equilibrium in a reversible eigensystem then also produces the wrong matrix exponential.

## Options

### Require a common equilibrium

Require every `Q_i` to have the same stationary distribution `pi`.  Then `mu(i,x) = alpha[i] pi[x]`, and the
standard switching rates are independent of `x`.  If the component models and `M` are reversible, the expanded
model is reversible.

This includes the classical rate-modulation and covarion models, but does not support temporal changes in character
frequencies.

### Keep autonomous switching and compute the actual equilibrium

Keep `M[i,j]` independent of `x`, construct the full expanded rate matrix, and solve `rho Q = 0` for its actual
equilibrium `rho`.  This preserves the interpretation of `M` as an autonomous regime process, and the regime marginal
remains `alpha`, but the conditional character distribution `rho(i,x) / alpha[i]` is generally not `pi_i`.  Probability
therefore bleeds from characters that are common in one regime into the same characters in regimes where they would
be rare in isolation.

The complete model will generally be nonreversible even when each `Q_i` and `M` is reversible.  This is a valid
model, but it has different semantics from a constructor that promises the component equilibrium frequencies.

To see why another equilibrium does not normally restore reversibility, suppose the complete model were reversible
with equilibrium `rho`.  Detailed balance on the switching edges, together with reversibility of `M`, would require

    rho(i,x) = alpha[i] p[x]

for one character distribution `p` shared by every connected regime.  Detailed balance on the edges within regime
`i` would then require `p` to be the reversible equilibrium `pi_i` of `Q_i`.  For irreducible component and switching
matrices, this is possible only when all the `pi_i` agree; reducible or disconnected cases can be exceptions.

The same obstruction appears in the four-state cycle

    (i,x) -> (i,y) -> (j,y) -> (j,x) -> (i,x).

Kolmogorov's cycle criterion requires the products of rates around this rectangle to agree in both directions.  The
switching rates cancel, leaving

    Q_i[x,y] Q_j[y,x] = Q_i[y,x] Q_j[x,y],

which, for reversible component matrices, requires

    pi_i[y] / pi_i[x] = pi_j[y] / pi_j[x].

Thus the `Q_i` remain reversible rate matrices in isolation, but `rho` restricted to regime `i` is not their reversible
equilibrium.  Their within-regime edges consequently carry stationary probability currents that participate in
circulation around mixed regime/character cycles.

### Use Whelan's equilibrium-preserving rates

Whelan multiplies a transition into regime `j` by the destination regime's frequency for the retained character:

    (i,x) -> (j,x) at M[i,j] pi_j[x].

When `M` is reversible with respect to `alpha`, these rates preserve `mu` and satisfy detailed balance across regime
transitions.  Component matrices need only be stationary, not reversible, for `mu` to remain stationary; a
nonreversible component makes the complete model nonreversible.

The drawback is that `M` behaves as an exchangeability rather than as the actual switching rate.  Even when all
regimes have the same nonuniform `pi`, switches occur more slowly while the process is in a rare character state, so
this construction does not reduce to the classical covarion generator.

### Use an equilibrium-preserving reversible bridge

Detailed balance does not uniquely determine how switching events are distributed among character states.  For each
pair of regimes, choose a probability distribution `b_ij` satisfying

    b_ij[x] >= 0
    sum_x b_ij[x] = 1
    b_ij[x] = b_ji[x].

Here `b_ij[x]` is the fraction of the equilibrium switching flow between regimes `i` and `j` that occurs while the
retained character is `x`.  Define

    (i,x) -> (j,x) at M[i,j] b_ij[x] / pi_i[x].

The equilibrium flow on this edge is

    mu(i,x) M[i,j] b_ij[x] / pi_i[x]
        = alpha[i] M[i,j] b_ij[x].

It is therefore balanced by the reverse flow when `M` is reversible.  Averaging the rate over `pi_i` gives exactly
`M[i,j]`, so `M` retains its interpretation as the mean switch rate.

When `pi_i = pi_j = pi`, choosing `b_ij = pi` recovers the classical state-independent rate `M[i,j]`.

#### Geometric bridge

A smooth and symmetric choice for different frequencies is the normalized geometric mean.  Define

    C_ij = sum_y sqrt(pi_i[y] pi_j[y])
    b_ij[x] = sqrt(pi_i[x] pi_j[x]) / C_ij.

`C_ij` is a measure of overlap between the two distributions.  The bridge assigns more switching throughput to
characters that are common in both regimes, rather than to characters whose frequencies are merely similar, and the
resulting rate is

    (i,x) -> (j,x) at M[i,j] sqrt(pi_j[x] / pi_i[x]) / C_ij.

If the distributions barely overlap, `C_ij` is small, and preserving the specified mean switching rate requires large
rates through the few character states they share.  If their supports are disjoint, a state-retaining
equilibrium-preserving switch is impossible.  When the distributions agree, `C_ij = 1` and the bridge reduces to the
classical state-independent switching rate.

The geometric bridge is smooth when all frequencies are positive, but remains a proposed construction rather than a
model selected by the literature reviewed below.

#### Metropolis bridge

Another construction treats `M[i,j]` as the rate of proposing a switch from `i` to `j` and accepts the proposal with
a continuous-time Metropolis correction:

    (i,x) -> (j,x) at M[i,j] min(1, pi_j[x] / pi_i[x]).

Its equilibrium flow for character `x` is

    alpha[i] M[i,j] min(pi_i[x], pi_j[x]),

which equals the reverse flow when `M` is reversible.  It also has a maximal-throughput interpretation: subject to
detailed balance and to neither adjusted direction exceeding its proposed rate, it admits the greatest possible flow
for each character.  When the component frequencies agree, every proposal is accepted and the construction reduces
to the classical generator.

Define the shared-mass overlap

    O_ij = sum_x min(pi_i[x], pi_j[x])
         = 1 - TV(pi_i, pi_j).

Here `TV` is the total-variation distance.  Without normalization, the mean realized switching rate is
`M[i,j] O_ij`; frequency mismatch therefore slows regime switching.  Dividing the Metropolis rate by `O_ij` instead
gives the normalized bridge

    b_ij[x] = min(pi_i[x], pi_j[x]) / O_ij,

which preserves the mean rate `M[i,j]` but allows some instantaneous rates to exceed it.

These endpoints can be interpolated with an overlap-compensation parameter `t`:

    (i,x) -> (j,x) at M[i,j] min(1, pi_j[x] / pi_i[x]) / O_ij^t
    mean rate(i -> j) = M[i,j] O_ij^(1-t),       0 <= t <= 1.

At `t = 0`, `M` is the proposal rate; at `t = 1`, `M` is the mean realized rate.  An estimated `t` is redundant if
every regime pair has a freely estimated reversible switching rate, because those rates can absorb `O_ij^(-t)`.
It can be identified only when `M` is fixed or constrained across pairs with different overlaps.

Unlike the geometric bridge, the Metropolis bridge is not differentiable where two component frequencies cross.
Both bridges require an explicit policy for zero frequencies, and no normalization can repair disjoint support.

The pairwise bridge assumes a reversible `M`.  A nonreversible `M` needs a more general allocation whose incoming
and outgoing switching flows balance separately for every character, such as using one common `b[x]` for all regime
edges.

### Let a regime switch also change the character

A switch can apply a transition kernel instead of retaining `x`.  The simple reset construction

    (i,x) -> (j,y) at M[i,j] pi_j[y]

preserves `mu` when `M` is reversible and leaves the mean regime-switching rates equal to `M`, but it permits a
simultaneous regime and character change.  More general kernels can retain the character more often while satisfying
the required balance equations.

This construction is mathematically standard for a process in a random environment, but its biological meaning and
its treatment when counting substitutions would need to be specified.

### Use a nonstationary rooted model

The model can start from a chosen distribution, including the product-form `mu`, without claiming that it is
stationary.  This permits directional changes in composition, but requires a rooted likelihood calculation and cannot
use reversible-model algorithms.

## Current direction

The equilibrium-preserving bridges have the most attractive combination of properties considered so far:

* they preserve the advertised component frequencies;
* they guarantee reversibility when `M` and all component matrices are reversible;
* they permit nonreversible component matrices while retaining a known stationary distribution;
* they reduce to the classical generator when component frequencies agree.

The normalized geometric and Metropolis bridges keep `M[i,j]` as the mean switching rate.  The unnormalized
Metropolis construction instead gives `M[i,j]` the concrete meaning of a proposal rate and allows frequency mismatch
to reduce realized switching.

Before implementation, the bridge and its normalization must be chosen and justified.  The API should also
distinguish this model from autonomous switching with a numerically computed equilibrium, because the constructions
give `M` different meanings.

Tests should include unequal-frequency components and check `mu Q = 0`, detailed balance when appropriate, the
matrix exponential, mean regime-switching rates, and exact reduction to the classical common-frequency generator.

## References

### Tuffley and Steel (1998)

[Modelling the Covarion Hypothesis of Nucleotide Substitution](https://doi.org/10.1016/S0025-5564(97)00081-3).
The model combines an observable substitution process with an independent on/off switch and writes the expanded
generator with state-independent switching blocks.  It supplies the classical behavior that a generalization should
recover when component equilibria agree.

### Galtier and Jean-Marie (2004)

[Markov-Modulated Markov Chains and the Covarion Process of Molecular Evolution](https://doi.org/10.1089/cmb.2004.11.727).
The paper formulates covarion processes as continuous-time Markov-modulated Markov chains and generalizes rate
modulation to multiple classes.  Its focus is rate variation over a shared character process and efficient
diagonalization of the expanded generator.

### Wang et al. (2007)

[Testing for Covarion-like Evolution in Protein Sequences](https://doi.org/10.1093/molbev/msl155).  The general
covarion model combines on/off switching with switches among multiple active rate classes.  Class switches retain the
character and change its rate, so the model continues to use a common character equilibrium.

### Whelan (2008)

[Spatial and Temporal Heterogeneity in Nucleotide Sequence Evolution](https://doi.org/10.1093/molbev/msn119).
Whelan allows regimes to have different nucleotide frequencies and multiplies cross-regime rates by the destination
frequency to enforce detailed balance.  This preserves product-form equilibrium but makes switching depend on the
retained nucleotide even when all regime frequencies agree.

### Economou (2005)

[Generalized Product-Form Stationary Distributions for Markov Chains in Random Environments with Queueing
Applications](https://doi.org/10.1239/aap/1113402405).  The paper gives necessary and sufficient conditions for
product-form stationary distributions in continuous-time random environments.  For switches that retain the process
state, its results show why independently switching regimes generally require a common conditional equilibrium.

### Ephraim and Roberts (2009)

[An EM Algorithm for Markov Modulated Markov Processes](https://doi.org/10.1109/TSP.2008.2007919).  This is a
general treatment of an observable continuous-time Markov process whose generator is selected by a hidden
continuous-time Markov chain.  It illustrates the autonomous-switching interpretation without requiring the joint
stationary distribution to factor into the stationary distributions of the two component processes.

### Baele et al. (2021)

[Markov-Modulated Continuous-Time Markov Chains to Identify Site- and Branch-Specific Evolutionary Variation in
BEAST](https://doi.org/10.1093/sysbio/syaa037).  The paper implements a flexible expanded-state phylogenetic model
with state-independent regime switches and permits component models with different stationary distributions.  Its
product-form stationary-distribution argument appears to omit the unequal component-frequency factors and therefore
has the same problem identified in the current BAli-Phy construction.
