#ifndef EFFECTS_H
#define EFFECTS_H
#include "effect.H"

class register_random_variable: public effect
{
    int random_variable_reg;

public:
    register_random_variable* clone() const {return new register_random_variable(*this);}

    bool operator==(const register_random_variable&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_random_variable(int r);
};

class register_likelihood: public effect
{
    int likelihood_reg;

public:
    register_likelihood* clone() const {return new register_likelihood(*this);}

    bool operator==(const register_likelihood&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_likelihood(int r);
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
