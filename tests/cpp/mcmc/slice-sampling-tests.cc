#include "mcmc/slice-sampling.H"
#include "test-util.H"
#include "util/rng.H"

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
    check_samples_within_existing_exceptional_rank();
}
