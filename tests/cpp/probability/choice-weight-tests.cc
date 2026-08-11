#include "probability/choose.H"
#include "test-util.H"
#include "util/rng.H"

#include <cmath>
#include <limits>
#include <optional>
#include <type_traits>
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

    // DensityRank is a total lexicographic order: NaNs, then infinities, then zeros.
    void check_density_rank_order()
    {
        auto neutral = weight(1);
        BALI_PHY_TEST_CHECK(weight(1, 0, 0, -1).rank() < neutral.rank());
        BALI_PHY_TEST_CHECK(weight(1, 0, 0, 1).rank() > neutral.rank());
        BALI_PHY_TEST_CHECK(weight(1, 0, -1).rank() < neutral.rank());
        BALI_PHY_TEST_CHECK(weight(1, 0, 1).rank() > neutral.rank());
        BALI_PHY_TEST_CHECK(weight(1, -1).rank() < neutral.rank());
        BALI_PHY_TEST_CHECK(weight(1, 1).rank() > neutral.rank());
        BALI_PHY_TEST_CHECK(neutral.rank() == weight(2).rank());

        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 0, 0, 1), weight(1, 4, 3, 0)));
        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 0, 2, 1), weight(1, 5, 1, 1)));
        BALI_PHY_TEST_CHECK(dominated_by(weight(1, 3, 2, 1), weight(1, 2, 2, 1)));
    }

    // ChoiceWeight multiplication combines independent symbolic factors, and
    // its ordering reverses defect rank before comparing finite coefficients.
    void check_weight_multiplication_and_order()
    {
        auto product = weight(2, 1, 2, 1) * weight(3, 4, 5, 2);
        auto expected = weight(6, 5, 7, 3);
        BALI_PHY_TEST_CHECK(product.rank() == expected.rank());
        check_close(double(product / expected), 1.0);

        BALI_PHY_TEST_CHECK(weight(1) > weight(100, 1));
        BALI_PHY_TEST_CHECK(weight(3, 1) > weight(2, 1));
        BALI_PHY_TEST_CHECK(weight(2, 1) == weight(2, 1));
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

    // Division projects every defect category around the neutral rank, while
    // equal ranks retain the ordinary finite coefficient ratio.
    void check_division_projection()
    {
        auto neutral = weight(1);
        check_close(double(weight(0.25) / neutral), 0.25);

        BALI_PHY_TEST_CHECK(double(weight(1, 0, 0, 1) / neutral) == 0.0);
        BALI_PHY_TEST_CHECK(double(weight(1, 0, 1) / neutral) == 0.0);
        BALI_PHY_TEST_CHECK(double(weight(1, 1) / neutral) == 0.0);

        BALI_PHY_TEST_CHECK(std::isinf((weight(1, 0, 0, -1) / neutral).log()));
        BALI_PHY_TEST_CHECK(std::isinf((weight(1, 0, -1) / neutral).log()));
        BALI_PHY_TEST_CHECK(std::isinf((weight(1, -1) / neutral).log()));
    }

    // The actual categorical sampler must never select a dominated candidate.
    void check_choice_ignores_dominated_weights()
    {
        myrand_init(781930);
        std::vector<ChoiceWeight> weights{weight(1, 2), weight(1), weight(1, 1)};
        for(int i = 0; i < 100; i++)
            BALI_PHY_TEST_CHECK(choose(weights) == 1);

        std::vector<int> ordinary_choices;
        myrand_init(81723);
        for(int i = 0; i < 100; i++)
            ordinary_choices.push_back(choose(std::vector<log_double_t>{2, 3}));

        // Equal exceptional ranks use the same coefficient thresholds as an
        // ordinary categorical draw, without first normalizing the stratum.
        myrand_init(81723);
        std::vector<ChoiceWeight> equal_rank{weight(2, 1), weight(3, 1)};
        for(int i = 0; i < ordinary_choices.size(); i++)
            BALI_PHY_TEST_CHECK(choose(equal_rank) == ordinary_choices[i]);
    }

    // Choice algorithms retain ChoiceWeight for weight arithmetic and project
    // only their returned categorical or MH probabilities to log_double_t.
    void check_choice_weight_probabilities()
    {
        std::vector<ChoiceWeight> finite{weight(2), weight(3)};
        auto categorical_probability = choose_P(0, finite);
        static_assert(std::is_same_v<decltype(categorical_probability), log_double_t>);
        check_close(double(categorical_probability), 0.4);

        std::vector<ChoiceWeight> exact_zeros{ChoiceWeight(0), ChoiceWeight(0)};
        check_close(double(choose_P(0, exact_zeros)), 0.5);

        auto forward = choose_MH_P(0, 1, finite);
        auto reverse = choose_MH_P(1, 0, finite);
        check_close(double(forward), 1.0);
        check_close(double(reverse), 2.0 / 3.0);

        std::vector<ChoiceWeight> exceptional{weight(2, 1), weight(3)};
        BALI_PHY_TEST_CHECK(choose_MH(0, exceptional) == 1);
        check_close(double(choose_MH_P(0, 1, exceptional)), 1.0);
        check_close(double(choose_MH_P(1, 0, exceptional)), 0.0);

        using available_weight = Availability<ChoiceWeight>;
        std::vector<available_weight> available_weights{
            unavailable, available(weight(2, 1)), available(weight(3, 1))
        };
        auto transition_probability = choose_MH_P(1, 2, available_weights);
        static_assert(std::is_same_v<decltype(transition_probability), Availability<log_double_t>>);
        BALI_PHY_TEST_CHECK(transition_probability);
        check_close(double(*transition_probability), 1.0);
        auto absent_destination = choose_MH_P(1, 0, available_weights);
        BALI_PHY_TEST_CHECK(absent_destination and *absent_destination == 0.0);
    }

    // Availability arithmetic protects the distinction between an unavailable
    // term and an available zero; this is obsolete only if choices use another
    // representation with the same two levels of absence.
    void check_availability_arithmetic()
    {
        auto absent = Availability<log_double_t>(unavailable);
        auto zero = available(log_double_t(0));
        auto two = available(log_double_t(2));

        BALI_PHY_TEST_CHECK(absent + two == two);
        BALI_PHY_TEST_CHECK(not (absent * two));
        BALI_PHY_TEST_CHECK(absent < zero);

        auto absent_numerator = absent / two;
        BALI_PHY_TEST_CHECK(absent_numerator and *absent_numerator == 0.0);
        BALI_PHY_TEST_CHECK(not (two / absent));
        BALI_PHY_TEST_CHECK(not (absent / absent));

        auto half = available(log_double_t(0.5));
        BALI_PHY_TEST_CHECK(complement(half));
        check_close(double(*complement(half)), 0.5);
        BALI_PHY_TEST_CHECK(not complement(absent));

        auto choice_weight = available(weight(3));
        auto left_product = log_double_t(2) * choice_weight;
        auto right_product = choice_weight * log_double_t(2);
        BALI_PHY_TEST_CHECK(left_product and right_product);
        check_close(double(*left_product / weight(1)), 6.0);
        check_close(double(*right_product / weight(1)), 6.0);
    }

    // Availability-aware categorical choice must retain caller indices while
    // excluding unavailable entries; this becomes obsolete only with a shared
    // indexed choice abstraction that provides the same guarantee.
    void check_available_choices()
    {
        using available_weight = Availability<log_double_t>;

        std::vector<available_weight> weights{
            unavailable, available(log_double_t(2)), unavailable, available(log_double_t(3))
        };
        auto p0 = choose_P(0, weights);
        auto p1 = choose_P(1, weights);
        auto p3 = choose_P(3, weights);
        BALI_PHY_TEST_CHECK(p0 and *p0 == 0.0);
        BALI_PHY_TEST_CHECK(p1);
        BALI_PHY_TEST_CHECK(p3);
        check_close(double(*p1), 0.4);
        check_close(double(*p3), 0.6);

        myrand_init(930152);
        for(int i = 0; i < 100; i++)
        {
            auto choice = ::choose(weights);
            BALI_PHY_TEST_CHECK(choice and (*choice == 1 or *choice == 3));
        }

        std::vector<available_weight> absent(3);
        BALI_PHY_TEST_CHECK(not choose(absent));
        BALI_PHY_TEST_CHECK(not choose_P(1, absent));
        BALI_PHY_TEST_CHECK(not choose_MH(1, absent));
        BALI_PHY_TEST_CHECK(not choose_MH_P(1, 2, absent));
    }

    // Unavailable MH destinations have zero transition probability without
    // changing the kernel on available entries; this is obsolete only if the
    // MH selector adopts another representation of unavailable candidates.
    void check_available_MH_choices()
    {
        using available_weight = Availability<log_double_t>;

        std::vector<available_weight> one_choice{
            unavailable, available(log_double_t(1)), unavailable
        };
        auto selected = choose_MH(1, one_choice);
        BALI_PHY_TEST_CHECK(selected and *selected == 1);
        BALI_PHY_TEST_CHECK(not choose_MH(0, one_choice));

        auto absent_destination = choose_MH_P(1, 0, one_choice);
        BALI_PHY_TEST_CHECK(absent_destination and *absent_destination == 0.0);
        BALI_PHY_TEST_CHECK(not choose_MH_P(0, 1, one_choice));

        // Unlike an absent current entry, a present zero-weight current can
        // escape to an option with positive weight under the existing kernel.
        std::vector<available_weight> zero_current{
            available(log_double_t(0)), available(log_double_t(1))
        };
        selected = choose_MH(0, zero_current);
        BALI_PHY_TEST_CHECK(selected and *selected == 1);
        auto escape_probability = choose_MH_P(0, 1, zero_current);
        BALI_PHY_TEST_CHECK(escape_probability and *escape_probability == 1.0);

        std::vector<available_weight> weights{
            unavailable, available(log_double_t(2)), available(log_double_t(3))
        };
        std::vector<log_double_t> compact{log_double_t(2), log_double_t(3)};
        auto available_probability = choose_MH_P(1, 2, weights);
        BALI_PHY_TEST_CHECK(available_probability);
        BALI_PHY_TEST_CHECK(*available_probability == choose_MH_P(0, 1, compact));
    }
}

// Exercise the enduring algebraic invariants independently of an MCMC model.
int main()
{
    check_prob_density_zero();
    check_finite_weights();
    check_zero_order_addition();
    check_density_rank_order();
    check_weight_multiplication_and_order();
    check_common_symbolic_factors();
    check_sum_order_independence();
    check_division_projection();
    check_choice_ignores_dominated_weights();
    check_choice_weight_probabilities();
    check_availability_arithmetic();
    check_available_choices();
    check_available_MH_choices();
}
