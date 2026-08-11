#ifndef CHOICE_WEIGHT_H
#define CHOICE_WEIGHT_H

#include "util/math/ProbDensity.H"

#include <cassert>
#include <cmath>
#include <compare>
#include <limits>
#include <ostream>
#include <vector>

// Rank a density first by MH pathologies and then by its asymptotic order of zero.
// A smaller rank is preferred: fewer NaNs, then fewer positive infinities, then
// fewer zero factors.
class DensityRank
{
    int nans_ = 0;
    double infs_ = 0;
    double zero_order_ = 0;

public:
    explicit DensityRank(const LogDensity& density)
        :nans_(density.nans()), infs_(density.infs()), zero_order_(density.neginfs())
    {
        assert(not std::isnan(infs_));
        assert(not std::isnan(zero_order_));
    }

    constexpr DensityRank() = default;

    bool operator==(const DensityRank&) const = default;

    // The rank fields exclude NaN, so the MH preference rules form a total
    // lexicographic order even though two of the fields use floating point.
    std::strong_ordering operator<=>(const DensityRank& other) const
    {
        if (nans_ != other.nans_)
            return nans_ <=> other.nans_;
        if (infs_ < other.infs_)
            return std::strong_ordering::less;
        if (infs_ > other.infs_)
            return std::strong_ordering::greater;
        if (zero_order_ < other.zero_order_)
            return std::strong_ordering::less;
        if (zero_order_ > other.zero_order_)
            return std::strong_ordering::greater;
        return std::strong_ordering::equal;
    }
};

// A probability density interpreted under DensityRank.  Addition supplies the
// preferred-stratum sum needed by discrete choices, multiplication combines
// factors, and division projects two ranked densities to an ordinary relative
// weight.
class ChoiceWeight
{
    ProbDensity weight_;

public:
    explicit ChoiceWeight(const ProbDensity& weight):weight_(weight) {}
    explicit ChoiceWeight(const log_double_t& weight):weight_(weight) {}
    explicit ChoiceWeight(double weight):weight_(weight) {}

    DensityRank rank() const {return DensityRank(weight_.log());}
    const ProbDensity& density() const {return weight_;}

    // Choice weights are ordered by preference first and by their finite
    // coefficient second.  DensityRank uses the opposite direction because a
    // smaller defect rank is preferred and therefore represents more weight.
    friend std::strong_ordering operator<=>(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        assert(x.weight_.log().isvalid());
        assert(y.weight_.log().isvalid());
        auto rank_order = y.rank() <=> x.rank();
        if (rank_order != 0)
            return rank_order;

        const double x_coefficient = x.weight_.log().ones();
        const double y_coefficient = y.weight_.log().ones();
        assert(std::isfinite(x_coefficient));
        assert(std::isfinite(y_coefficient));
        if (x_coefficient < y_coefficient)
            return std::strong_ordering::less;
        if (x_coefficient > y_coefficient)
            return std::strong_ordering::greater;
        return std::strong_ordering::equal;
    }

    friend bool operator==(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        return (x <=> y) == 0;
    }

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

        LogDensity total = x.weight_.log();
        total.ones() = logsum(x.weight_.log().ones(), y.weight_.log().ones());
        return ChoiceWeight(exp_to_log_space(total));
    }

    // Multiplication combines density factors.  In particular,
    // ChoiceWeight(uniform()) * total supplies the threshold used by choose.
    friend ChoiceWeight operator*(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        return ChoiceWeight(x.weight_ * y.weight_);
    }

    // Project ranked densities to an ordinary relative weight: a dominated
    // numerator is zero, a preferred numerator is infinite, and equal-rank
    // symbolic factors cancel to leave the finite coefficient ratio.
    friend log_double_t operator/(const ChoiceWeight& x, const ChoiceWeight& y)
    {
        if (dominated_by(x, y))
            return 0.0;
        if (dominated_by(y, x))
            return exp_to_log_space(std::numeric_limits<double>::infinity());

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
