#include "mcmc/slice-sampling.H"
#include "test-util.H"
#include "util/rng.H"

#include <cmath>
#include <limits>

namespace
{
    // A deterministic-shaped density exposes the scalar sampler without requiring
    // a computation context or an MCMC model.
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

        std::optional<ProbDensity> operator()(double x) override
        {
            evaluations++;
            evaluated_nonfinite_coordinate |= not std::isfinite(x);
            x_ = x;
            return operator()();
        }

        ProbDensity operator()() override
        {
            return base_density_ * ProbDensity(exp_to_log_space(-0.5 * x_ * x_));
        }

        double current_value() const override {return x_;}

        void reset() override
        {
            resets++;
            x_ = initial_x_;
        }
    };

    void require_declined(test_slice_function& function, double w)
    {
        double initial_x = function.current_value();
        double sampled = slice_sample(function, w, 10);

        BALI_PHY_TEST_CHECK((std::isinf(initial_x) and std::isinf(sampled)) or sampled == initial_x);
        BALI_PHY_TEST_CHECK(function.resets == 1);
        BALI_PHY_TEST_CHECK(function.evaluations == 0);
    }

    void check_declines_nonfinite_start()
    {
        test_slice_function function(std::numeric_limits<double>::infinity());
        require_declined(function, 1);
    }

    void check_declines_nonfinite_window()
    {
        test_slice_function function(0);
        require_declined(function, std::numeric_limits<double>::infinity());
    }

    void check_declines_nan_bound()
    {
        test_slice_function function(0);
        function.set_lower_bound(std::numeric_limits<double>::quiet_NaN());
        require_declined(function, 1);
    }

    void check_samples_within_existing_exceptional_rank()
    {
        myrand_init(917315);
        test_slice_function function(0, ProbDensity(0));
        function.set_lower_bound(-4);
        function.set_upper_bound(4);

        double sampled = slice_sample(function, 1, 10);

        BALI_PHY_TEST_CHECK(std::isfinite(sampled));
        BALI_PHY_TEST_CHECK(function.current_value() == sampled);
        BALI_PHY_TEST_CHECK(function.evaluations > 0);
        BALI_PHY_TEST_CHECK(not function.evaluated_nonfinite_coordinate);
        BALI_PHY_TEST_CHECK(function().log().neginfs() == 1);
    }
}

int main()
{
    check_declines_nonfinite_start();
    check_declines_nonfinite_window();
    check_declines_nan_bound();
    check_samples_within_existing_exceptional_rank();
}
