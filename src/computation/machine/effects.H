#ifndef EFFECTS_H
#define EFFECTS_H
#include "effect.H"

struct register_prob
{
    int r_dist;
    int r_prob;
    log_double_t prob;

    bool operator==(const register_prob&) const;

    register_prob(int r1, int r2, log_double_t);
};


struct register_prior: public register_prob, public effect
{
    register_prior* clone() const {return new register_prior(*this);}

    bool operator==(const register_prior&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    using register_prob::register_prob;
};

struct register_likelihood: public register_prob, public effect
{
    register_likelihood* clone() const {return new register_likelihood(*this);}

    bool operator==(const register_likelihood&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    using register_prob::register_prob;
};

struct register_transition_kernel: public effect
{
    double rate;
    int kernel_reg;

    register_transition_kernel* clone() const {return new register_transition_kernel(*this);}

    bool operator==(const register_transition_kernel&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    register_transition_kernel(double r, int k);
};

#endif
