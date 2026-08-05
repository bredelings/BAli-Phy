#ifndef CHOICE_WEIGHT_H
#define CHOICE_WEIGHT_H

#include "util/math/ProbDensity.H"

#include <compare>
#include <limits>
#include <ostream>
#include <vector>

// Rank a density first by MH pathologies and then by its asymptotic order of zero.
// A smaller rank is preferred: fewer NaNs, then fewer positive infinities, then
// fewer zero factors.
struct ChoiceRank
{
    int nans = 0;
    double infs = 0;
    double zero_order = 0;

    explicit ChoiceRank(const LogDensity& density)
        :nans(density.nans()), infs(density.infs()), zero_order(density.neginfs())
    {}

    constexpr ChoiceRank() = default;

    bool operator==(const ChoiceRank&) const = default;

    // Lexicographic comparison applies the MH preference rules in order: NaNs,
    // positive infinities, and finally the asymptotic order of zero.
    std::partial_ordering operator<=>(const ChoiceRank& other) const
    {
        if (nans != other.nans)
            return nans <=> other.nans;
        if (infs != other.infs)
            return infs <=> other.infs;
        return zero_order <=> other.zero_order;
    }
};

// A probability density interpreted as one candidate in a discrete choice.
// Addition keeps only the preferred rank and adds finite coefficients within
// that rank.  Division projects the result back to an ordinary log-space weight.
class ChoiceWeight
{
    ProbDensity weight_;

public:
    explicit ChoiceWeight(const ProbDensity& weight):weight_(weight) {}
    explicit ChoiceWeight(double weight):weight_(weight) {}

    ChoiceRank rank() const {return ChoiceRank(weight_.log());}
    const ProbDensity& density() const {return weight_;}

    friend bool dominated_by(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        return x.rank() > y.rank();
    }

    // In an ordered limiting argument, a less-preferred rank vanishes; equal
    // ranks retain their finite coefficients and therefore use log-sum-exp.
    friend ChoiceWeight operator+(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        if (dominated_by(x, y))
            return y;
        if (dominated_by(y, x))
            return x;

        auto order = x.rank() <=> y.rank();
        if (order == std::partial_ordering::unordered)
            return ChoiceWeight(exp_to_log_space(logNan()));

        LogDensity total = x.weight_.log();
        total.ones() = logsum(x.weight_.log().ones(), y.weight_.log().ones());
        return ChoiceWeight(exp_to_log_space(total));
    }

    // Project a ratio to an ordinary weight: a dominated numerator is zero,
    // while equal-rank symbolic factors cancel and leave a coefficient ratio.
    friend log_double_t operator/(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        if (dominated_by(x, y))
            return 0.0;
        if (dominated_by(y, x))
            return exp_to_log_space(std::numeric_limits<double>::infinity());

        auto order = x.rank() <=> y.rank();
        if (order == std::partial_ordering::unordered)
            return log_double_t(x.weight_ / y.weight_);

        return exp_to_log_space(x.weight_.log().ones() - y.weight_.log().ones());
    }

    friend std::ostream& operator<<(std::ostream& out, const ChoiceWeight& weight)
    {
        return out<<weight.weight_;
    }
};

// Normalize a nonempty collection only after its complete preferred-stratum
// total is known.  Dominated candidates divide to ordinary zero weights.
inline std::vector<log_double_t> choice_probabilities(const std::vector<ChoiceWeight>& weights)
{
    assert(not weights.empty());

    ChoiceWeight total = weights.front();
    for(unsigned i = 1; i < weights.size(); i++)
        total = total + weights[i];

    std::vector<log_double_t> probabilities;
    probabilities.reserve(weights.size());
    for(const auto& weight: weights)
        probabilities.push_back(weight / total);

    return probabilities;
}

#endif
