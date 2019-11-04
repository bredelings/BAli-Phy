#ifndef PROB_RATIOS_H
#define PROB_RATIOS_H

#include "util/math/log-double.H"

struct prob_ratios_t
{
    bool variables_changed = false;

    log_double_t prior_ratio = 1.0;
    log_double_t likelihood_ratio = 1.0;

    log_double_t total_ratio() const {return prior_ratio * likelihood_ratio;}
};

#endif
