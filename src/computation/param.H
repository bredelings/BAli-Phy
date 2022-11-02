#ifndef PARAM_H
#define PARAM_H

#include <optional>
#include "computation/expression/expression_ref.H"
#include "computation/expression/reg_var.H"
#include "computation/context.H"
#include "util/bounds.H"

enum class eval_strategy {unset, unchangeable, precomputed, changeable};

class context_ptr
{
    context_ref C;

    // This reg will never point to an index_var_no_force, since it is the result of evaluation.
    // Therefore, this reg will never be deleted behind our back!

    // But it could point to an index-var-with-force.  So we do a follow_index_var( )
    // before we look into the closure via operator[](int), size(), and head().

    int reg;
public:
    context_ptr result() const;
    std::optional<context_ptr> modifiable() const;
    void set_value(const expression_ref& v);

    void move_to_result();
    bool move_to_modifiable();

    int size() const;
    context_ptr operator[](int i) const;
    expression_ref head() const;
    expression_ref value() const;
    context_ptr list_element(int i) const;
    EVector list_to_vector() const;
    std::vector<context_ptr> list_elements() const;

    int get_reg() const {return reg;}
    context_ref& get_context() {return C;}

    context_ptr& operator=(const context_ptr&) = default;
    context_ptr& operator=(context_ptr&&) = default;

    context_ptr(const context_ptr&) = default;
    context_ptr(context_ptr&&) = default;

    context_ptr(const context_ref& c, int i);
};

class param
{
    std::optional<int> reg;
    std::optional<int> head;
    std::optional<expression_ref> value;
    eval_strategy strategy=eval_strategy::unset;

public:
    expression_ref ref(const context_ref& C) const;

    expression_ref get_value(const context_ref& C) const;
    void set_value(context_ref& C, const expression_ref& v) const;

    std::optional<expression_ref> constant_value() const {return value;}
    std::optional<int> is_modifiable(const context_ref& C) const;

    param& operator=(const param&) = default;

    explicit operator bool() const {return head or value;}

    param() {}
    param(const param&) = default;
    param(const context_ptr& p, eval_strategy s=eval_strategy::changeable):reg(p.get_reg()), strategy(s) {}
    param(const reg_var& rv, eval_strategy s=eval_strategy::changeable):reg(rv.target), strategy(s) {}
    param(int h, eval_strategy s=eval_strategy::changeable)
        :head(h), strategy(s)
        {}
    param(const expression_ref& v):value(v) {}
};

param get_param(context_ref& C, const expression_ref& E, eval_strategy s = eval_strategy::changeable);

std::vector<param> get_params_from_list_(context_ref& C, const expression_ref& list, std::optional<int> check_size = {});
std::vector<param> get_params_from_list(context_ref& C, const expression_ref& list, std::optional<int> check_size = {});
std::vector<param> get_params_from_array_(context_ref& C, const expression_ref& array, std::optional<int> check_size = {});
std::vector<param> get_params_from_array(context_ref& C, const expression_ref& array, std::optional<int> check_size = {});
#endif
