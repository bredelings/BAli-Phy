#ifndef OCCURRENCE_INFO_H
#define OCCURRENCE_INFO_H

enum class amount_t {Unknown, None, Once, Many};

enum class var_context {unknown, argument};

struct occurrence_info
{
    amount_t work_dup = amount_t::Unknown;

    amount_t code_dup = amount_t::Unknown;

    var_context context = var_context::unknown;

    bool is_loop_breaker = false;

    bool top_level = false;

    bool is_exported = false;

    bool pre_inline() const;
};

#endif
