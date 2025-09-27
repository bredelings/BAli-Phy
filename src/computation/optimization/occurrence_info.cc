#include "occurrence_info.H"

bool occurrence_info::pre_inline() const
{
    if (work_dup != amount_t::Once and work_dup != amount_t::None) return false;
    if (code_dup != amount_t::Once and code_dup != amount_t::None) return false;
    if (is_loop_breaker) return false;
    return true;
}

