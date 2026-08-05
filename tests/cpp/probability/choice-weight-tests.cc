#include "probability/choose.H"
#include "test-util.H"
#include "util/rng.H"

#include <cmath>
#include <limits>
#include <vector>

// Protect symbolic normalization rules that ordinary finite-weight tests cannot exercise.
// This suite becomes redundant if ProbDensity itself acquires these choice semantics.
namespace
{
    // Build symbolic weights directly so deterministic tests can cover every rank.
    ChoiceWeight weight(double coefficient, double zero_order = 0, double infs = 0, int nans = 0)
    {
        return ChoiceWeight(exp_to_log_space(
            LogDensity(zero_order, std::log(coefficient), infs, nans)));
    }

    // Compare ordinary projected probabilities tightly enough to detect algebra changes.
    void check_close(double observed, double expected)
    {
        bali_phy_test::require(std::abs(observed - expected) < 1.0e-12,
                               "choice probability differs from its expected value");
    }

    // Protect LogNum's representation-specific zero construction and recognition;
    // this is redundant if ProbDensity stops using LogNum.
    void check_prob_density_zero()
    {
        ProbDensity default_zero;
        BALI_PHY_TEST_CHECK(default_zero.is_zero());
        BALI_PHY_TEST_CHECK(default_zero.log().neginfs() == 1);

        ProbDensity assigned_zero = 1;
        assigned_zero = 0;
        BALI_PHY_TEST_CHECK(assigned_zero.is_zero());

        ProbDensity repeated_zero = default_zero * assigned_zero;
        BALI_PHY_TEST_CHECK(repeated_zero.log().neginfs() == 2);
        BALI_PHY_TEST_CHECK(repeated_zero.is_zero());
        BALI_PHY_TEST_CHECK(not ProbDensity(1).is_zero());
    }

    // Finite ChoiceWeights must reduce to ordinary categorical normalization.
    void check_finite_weights()
    {
        std::vector<ChoiceWeight> weights{weight(2), weight(3)};
        auto probabilities = choice_probabilities(weights);
        check_close(double(probabilities[0]), 0.4);
        check_close(double(probabilities[1]), 0.6);
    }

    // Higher orders of zero vanish, while equal orders retain coefficient ratios.
    void check_zero_order_addition()
    {
        auto higher_order = weight(2, 2);
        auto lower_order = weight(3, 1);
        auto total = higher_order + lower_order;
        BALI_PHY_TEST_CHECK(total.rank() == lower_order.rank());
        check_close(double(lower_order / total), 1.0);
        check_close(double(higher_order / total), 0.0);

        auto equal_order_total = weight(2, 1) + weight(3, 1);
        check_close(double(weight(2, 1) / equal_order_total), 0.4);
        check_close(double(weight(3, 1) / equal_order_total), 0.6);
    }

    // Defect comparison must apply the NaN, infinity, and zero hierarchy in order.
    void check_defect_priority()
    {
        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 0, 0, 1), weight(1, 4, 3, 0)));
        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 0, 2, 1), weight(1, 5, 1, 1)));
        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 3, 2, 1), weight(1, 2, 2, 1)));
    }

    // Symbolic factors shared by every candidate must cancel during normalization.
    void check_common_symbolic_factors()
    {
        auto common = exp_to_log_space(LogDensity(2, std::log(7.0), 1, 1));
        std::vector<ChoiceWeight> weights{
            ChoiceWeight(common * weight(2).density()),
            ChoiceWeight(common * weight(3).density())
        };
        auto probabilities = choice_probabilities(weights);
        check_close(double(probabilities[0]), 0.4);
        check_close(double(probabilities[1]), 0.6);
    }

    // Preferred-stratum addition must be associative and independent of input order.
    void check_sum_order_independence()
    {
        auto first = weight(2, 1);
        auto second = weight(3, 1);
        auto dominated = weight(5, 2);

        auto left_grouped = (first + second) + dominated;
        auto right_grouped = first + (dominated + second);
        BALI_PHY_TEST_CHECK(left_grouped.rank() == right_grouped.rank());
        check_close(double(first / left_grouped), 0.4);
        check_close(double(first / right_grouped), 0.4);

        auto forward = choice_probabilities({first, second, dominated});
        auto reverse = choice_probabilities({dominated, second, first});
        check_close(double(forward[0]), double(reverse[2]));
        check_close(double(forward[1]), double(reverse[1]));
        check_close(double(forward[2]), double(reverse[0]));
    }

    // Division must project dominated and dominating ranks to zero and infinity.
    void check_division_range()
    {
        auto preferred = weight(2);
        auto dominated = weight(3, 1);
        BALI_PHY_TEST_CHECK(double(dominated / preferred) == 0.0);
        BALI_PHY_TEST_CHECK(std::isinf((preferred / dominated).log()));
    }

    // The actual categorical sampler must never select a dominated candidate.
    void check_choice_ignores_dominated_weights()
    {
        myrand_init(781930);
        std::vector<ChoiceWeight> weights{weight(1, 2), weight(1), weight(1, 1)};
        for(int i = 0; i < 100; i++)
            BALI_PHY_TEST_CHECK(choose(weights) == 1);
    }
}

// Exercise the enduring algebraic invariants independently of an MCMC model.
int main()
{
    check_prob_density_zero();
    check_finite_weights();
    check_zero_order_addition();
    check_defect_priority();
    check_common_symbolic_factors();
    check_sum_order_independence();
    check_division_range();
    check_choice_ignores_dominated_weights();
}
