#include "mcmc/slice-sampling.hh"
#include "mcmc/sample.hh"
#include "test-util.hh"
#include "util/rng.hh"

#include <cmath>
#include <limits>

namespace
{
    // Protect recoverable boundary rejection and exceptional-rank sampling, which full-program
    // tests cannot exercise deterministically.  This fake becomes redundant if equivalent direct
    // coverage moves into a standalone scalar-sampling component.
    class test_slice_function: public slice_function
    {
        double initial_x_;
        double x_;
        ProbDensity base_density_;

    public:
        bool fail_on_change = false;
        int evaluations = 0;
        int resets = 0;
        bool evaluated_nonfinite_coordinate = false;

        test_slice_function(double x, ProbDensity density = 1)
            :initial_x_(x), x_(x), base_density_(density)
        {}

        // Evaluate a candidate while recording whether a nonfinite coordinate reached the density.
        std::optional<ProbDensity> operator()(double x) override
        {
            evaluations++;
            evaluated_nonfinite_coordinate |= not std::isfinite(x);
            x_ = x;
            if (fail_on_change and x != initial_x_)
                throw variables_changed_exception("test density changed variables");
            return operator()();
        }

        // Return a Gaussian-shaped coefficient within the configured exceptional rank.
        ProbDensity operator()() override
        {
            return base_density_ * ProbDensity(exp_to_log_space(-0.5 * x_ * x_));
        }

        double current_value() const override {return x_;}

        // Restore the starting coordinate and record that the sampler declined or failed.
        void reset() override
        {
            resets++;
            x_ = initial_x_;
        }
    };

    // Require an invalid boundary to return the exact starting coordinate without evaluation.
    void require_declined(test_slice_function& function, double w)
    {
        double initial_x = function.current_value();
        double sampled = slice_sample(function, w, 10);

        BALI_PHY_TEST_CHECK(sampled == initial_x);
        BALI_PHY_TEST_CHECK(function.resets == 1);
        BALI_PHY_TEST_CHECK(function.evaluations == 0);
    }

    // Cover invalid starting coordinates, windows, and bounds through their common no-op contract.
    void check_declines_invalid_boundaries()
    {
        {
            test_slice_function function(std::numeric_limits<double>::infinity());
            require_declined(function, 1);
        }

        {
            test_slice_function function(0);
            require_declined(function, std::numeric_limits<double>::infinity());
        }

        {
            test_slice_function function(0);
            function.set_lower_bound(std::numeric_limits<double>::quiet_NaN());
            require_declined(function, 1);
        }
    }

    // On a shared full interval, equal-height starts accept the same first in-slice draw.
    // A failed density evaluation must restore the state, even after installing a trial value.
    void check_full_interval()
    {
        const double near_end = std::nextafter(1.0, 0.0);
        test_slice_function left(-near_end), right(near_end);
        left.set_lower_bound(-1);
        left.set_upper_bound(1);
        right.set_lower_bound(-1);
        right.set_upper_bound(1);

        myrand_init(917315);
        double sampled = slice_sample_full_interval(left);
        myrand_init(917315);
        BALI_PHY_TEST_CHECK(slice_sample_full_interval(right) == sampled);
        BALI_PHY_TEST_CHECK(-near_end < sampled and sampled < near_end);

        left.reset();
        left.fail_on_change = true;
        BALI_PHY_TEST_CHECK(slice_sample_full_interval(left) == -near_end);
        BALI_PHY_TEST_CHECK(left.current_value() == -near_end);
        BALI_PHY_TEST_CHECK(left.resets == 2);

        // Adding a tiny adjacent length can round back to the focal length at the upper bound.
        test_slice_function endpoint(1.0);
        endpoint.set_lower_bound(0.0);
        endpoint.set_upper_bound(1.0 + std::ldexp(1.0, -54));
        BALI_PHY_TEST_CHECK(slice_sample_full_interval(endpoint) == 1.0);
        BALI_PHY_TEST_CHECK(endpoint.resets == 1 and endpoint.evaluations == 0);
    }

    // Translation must preserve individual draws even at either Int limit. These small slices
    // expose rounding of adjacent integers and clipping of the grid before reverse bisection.
    void check_integer_slice()
    {
        using I = std::int64_t;
        constexpr I low = std::numeric_limits<I>::min(), high = std::numeric_limits<I>::max();
        for (auto method: {integer_slice_method::doubling, integer_slice_method::stepping_out})
        {
            for (I width: {I(1), I(3), high})
            {
                auto draw = [&](I base, int seed) {
                    myrand_init(seed);
                    return sample_integer_on_slice(base+1, {base, base+3}, method, width, [&](I k) {
                        BALI_PHY_TEST_CHECK(base <= k and k <= base+3);
                        return k != base+2;
                    }) - base;
                };
                bool moved = false;
                for (int seed=0; seed<64; ++seed)
                {
                    auto y = draw(0, seed);
                    BALI_PHY_TEST_CHECK(y == 0 or y == 1 or y == 3);
                    BALI_PHY_TEST_CHECK(draw(low, seed) == y and draw(high-3, seed) == y);
                    moved |= y != 1;
                }
                BALI_PHY_TEST_CHECK(moved);
            }
            BALI_PHY_TEST_CHECK(sample_integer_on_slice(high, {high, high}, method, 1,
                                                        [](I){return true;}) == high);
        }

        // From 0 only the grid with boundary 0 can expand towards 6. On this grid the
        // reverse block [4,8) cannot expand, even when the forward bracket reached 6.
        myrand_init(274);
        for (int i=0; i<256; ++i)
            BALI_PHY_TEST_CHECK(sample_integer_on_slice(0, {0, 6}, integer_slice_method::doubling,
                                                        4, [](I k){return k == 0 or k == 6;}) == 0);
    }

    // Ordinary branch lengths check both conserved sums. Unbalanced pairs expose loss of a
    // small residual when a rounded sum is used to reconstruct a neighboring length.
    void check_three_branch_slice_coordinates()
    {
        const double a = 0.25, b = 0.5, c = 0.125, u = 0.3;
        BALI_PHY_TEST_CHECK(u + branch_length_at_fixed_pair_sum(a, b, u) == a+b);
        BALI_PHY_TEST_CHECK(u + branch_length_at_fixed_pair_sum(a, c, u) == a+c);

        const double tiny = std::ldexp(1.0, -54);
        const double near_one = std::nextafter(1.0, 0.0);
        BALI_PHY_TEST_CHECK(branch_length_at_fixed_pair_sum(1, tiny, near_one) == (1-near_one) + tiny);
        BALI_PHY_TEST_CHECK(branch_length_at_fixed_pair_sum(tiny, 1, near_one) == (1-near_one) + tiny);
    }

    // Verify that a zero-density rank supports an actual move rather than a reset to the start.
    void check_samples_within_existing_exceptional_rank()
    {
        myrand_init(917315);
        test_slice_function function(0, ProbDensity(0));
        function.set_lower_bound(-4);
        function.set_upper_bound(4);

        double sampled = slice_sample(function, 1, 10);

        BALI_PHY_TEST_CHECK(std::isfinite(sampled));
        BALI_PHY_TEST_CHECK(sampled != 0);
        BALI_PHY_TEST_CHECK(function.current_value() == sampled);
        BALI_PHY_TEST_CHECK(function.evaluations > 0);
        BALI_PHY_TEST_CHECK(function.resets == 0);
        BALI_PHY_TEST_CHECK(not function.evaluated_nonfinite_coordinate);
        BALI_PHY_TEST_CHECK(function().log().neginfs() == 1);
    }
}

// Run the complete scalar slice boundary contract as one native test executable.
int main()
{
    check_declines_invalid_boundaries();
    check_full_interval();
    check_integer_slice();
    check_three_branch_slice_coordinates();
    check_samples_within_existing_exceptional_rank();
}
