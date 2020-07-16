#ifndef EFFECTS_H
#define EFFECTS_H
#include "effect.H"

struct register_random_variable: public effect
{
    int variable_reg;
    log_double_t pdf;

    register_random_variable* clone() const {return new register_random_variable(*this);}

    bool operator==(const register_random_variable&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_random_variable(int r, log_double_t pr);
};

struct register_likelihood: public effect
{
    int likelihood_reg;
    log_double_t likelihood;

    register_likelihood* clone() const {return new register_likelihood(*this);}

    bool operator==(const register_likelihood&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_likelihood(int r, log_double_t lk);
};

class register_transition_kernel: public effect
{
    int rate_reg;
    int kernel_reg;

public:
    register_transition_kernel* clone() const {return new register_transition_kernel(*this);}

    bool operator==(const register_transition_kernel&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_transition_kernel(int r1, int r2);
};

#endif
