#ifndef EFFECTS_H
#define EFFECTS_H
#include "effect.H"

struct register_prior: public effect
{
    int variable_reg;
    log_double_t pdf;

    register_prior* clone() const {return new register_prior(*this);}

    bool operator==(const register_prior&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_prior(int r, log_double_t pr);
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

struct register_transition_kernel: public effect
{
    double rate;
    int kernel_reg;

    register_transition_kernel* clone() const {return new register_transition_kernel(*this);}

    bool operator==(const register_transition_kernel&) const;
    bool operator==(const Object& O) const;

    std::string print () const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_transition_kernel(double r, int k);
};

struct in_edge: public effect
{
    int r_from_var;
    int s_to_dist;
    std::string role;

    in_edge* clone() const {return new in_edge(*this);}

    bool operator==(const in_edge&) const;
    bool operator==(const Object& O) const;

    std::string print() const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    in_edge(int i1, int i2, const std::string& s);
};


struct out_edge: public effect
{
    int s_from_dist;
    int r_to_var;

    out_edge* clone() const {return new out_edge(*this);}

    bool operator==(const out_edge&) const;
    bool operator==(const Object& O) const;

    std::string print() const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    out_edge(int i1, int i2);
};


struct register_dist: public effect
{
    std::string name;

    register_dist* clone() const {return new register_dist(*this);}

    bool operator==(const register_dist&) const;
    bool operator==(const Object& O) const;

    std::string print() const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    register_dist(const std::string& s);
};


struct dist_property: public effect
{
    int s_from_dist;
    int r_to_prop;
    std::string property;

    dist_property* clone() const {return new dist_property(*this);}

    bool operator==(const dist_property&) const;
    bool operator==(const Object& O) const;

    std::string print() const;

    void register_effect(reg_heap&,int) const;
    void unregister_effect(reg_heap&,int) const;

    dist_property(int i1, int i2, const std::string& s);
};


#endif
