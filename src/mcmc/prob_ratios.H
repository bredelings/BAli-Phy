#ifndef PROB_RATIOS_H
#define PROB_RATIOS_H

#include "util/math/ProbDensity.H"

struct prob_ratios_t
{
    bool variables_changed = false;

    ProbDensity prior_ratio = 1.0;
    ProbDensity likelihood_ratio = 1.0;

    ProbDensity total_ratio() const {return prior_ratio * likelihood_ratio;}

    void heat(double b)
    {
        if (b == 0.0)
            likelihood_ratio = 1;
        else if (b == 1.0)
            ;
        else
            likelihood_ratio = pow(likelihood_ratio, b);
    }
};

#endif
