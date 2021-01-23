#ifndef PARAM_H
#define PARAM_H

#include <optional>
#include "computation/expression/expression_ref.H"
#include "util/bounds.H"

class context_ref;

enum class eval_strategy {unset, unchangeable, precomputed, changeable};

class param
{
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
    param(int h, eval_strategy s=eval_strategy::changeable)
        :head(h), strategy(s)
        {}
    param(const expression_ref& v):value(v) {}
};

param get_param(context_ref& C, const expression_ref& E, eval_strategy s = eval_strategy::changeable);

std::vector<param> get_params_from_list(context_ref& C, const expression_ref& list, std::optional<int> check_size = {});
std::vector<param> get_params_from_array(context_ref& C, const expression_ref& array, std::optional<int> check_size = {});
#endif
