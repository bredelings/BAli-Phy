% Adding a new MCMC move to BAli-Phy

Most moves are currently defined in C++.  Moves are actually added to the sampler in [src/mcmc/setup.cc](https://github.com/bredelings/BAli-Phy/blob/master/src/mcmc/setup.cc).

## `MCMC::MoveAll`

You can add other moves as sub-moves to an `MCMC::MoveAll`:

``` C++
MCMC::MoveAll M;
M.add(weight, MCMC::MH_Move( Proposal2M(proposal, m_index, parameters), name) );
```

The weight determines how many times the sub-move is run each iteration.

## `MCMC::SingleMove`

To add a generic MCMC move, create an `MCMC::SingleMove` with one of the following constructors:

``` C++
SingleMove(void (*move)(owned_ptr<context>&,MoveStats&), const std::string& name);
SingleMove(void (*move)(owned_ptr<context>&,MoveStats&), const std::string& name, const std::string& attributes);
```

You can pass in a function with signature `void(owned_ptr<context>&,MoveStats&)` that performs the move.  This is how moves that alter alignments are defined.

We use an `owned_ptr<>` so that we can treat context& polymorphically.

## `MCMC::MH_Move`

The `MCMC::MH_Move` has the following constructors:

``` C++
MH_Move(const Proposal& P, const std::string& name);
MH_Move(const Proposal& P, const std::string& name, const std::string& attributes);
```

### Proposals

Proposals are defined in [src/mcmc/proposals.H](https://github.com/bredelings/BAli-Phy/blob/master/src/mcmc/proposals.H).

Proposals are generally defined as functions that alter the MCMC state and then return a proposal ratio:

``` C++
class Proposal: public Object {
public:
    Proposal* clone() const =0;
    virtual log_double_t operator()(context& P) const=0;
};
```

Here `context&` is the current state of the MCMC object.  The type `log_double_t` is a probability (or probability_density) represented on the log scale.

### Proposal2

The Proposal2 class has constructor:

``` C++
Proposal2(const Proposal_Fn& p, const std::vector<std::string>& s, const std::vector<std::string>& v, const context& P);
```

The names in `s` are names of variables to modify, and the names in `v` are names of keys to look up to find tunable parameters such as jump sizes.

### Proposal_Fn

The `Proposal_Fn` class represents an MCMC move that affects some number of variables `x`, with some number of tunable parameters `p`.

``` C++
class Proposal_Fn
{
public:
    virtual log_double_t operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const;
};
```

It is possible to compose `Proposal_Fn`s to create complex proposals, such as:

1. ``Reflect(bounds, shift_cauchy)``
2. ``log_scaled(between(-20, 20, shift_cauchy))``
3. ``log_scaled(between(-20, 20, shift_gaussian))``

