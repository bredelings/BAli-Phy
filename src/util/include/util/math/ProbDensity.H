#ifndef PROBDENSITY_H
#define PROBDENSITY_H

#include "util/math/log-double.H"
#include "util/math/LogDensity.h"

template <>
struct LogNumTraits<LogDensity>
{
    static constexpr LogDensity zero_log_value() {return logZero();}

    // A symbolic product remains zero with any multiplicity of zero factors,
    // provided collapsing its accumulated defects has the value log(0).
    static bool is_zero(const LogDensity& value)
    {
        return static_cast<double>(value) == -infinity<double>;
    }
};

typedef LogNum<LogDensity> ProbDensity;

#endif
