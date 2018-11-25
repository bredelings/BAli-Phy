#ifndef PROB_RATIOS_H
#define PROB_RATIOS_H

#include "math/log-double.H"

struct prob_ratios_t
{
    log_double_t prior_ratio;
    log_double_t likelihood_ratio;
    log_double_t total_ratio() const {return prior_ratio * likelihood_ratio;}
    bool variables_changed = false;
};

#endif
