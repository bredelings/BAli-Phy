#include "computation/runtime/ast.hh"
#include "test-util.hh"
#include <limits>

namespace bali_phy_test
{
namespace
{
    // Check structural equality, NaN policy, and log-number representation hidden from Haskell tests.
    // Remove the bridge check if Log Double no longer shares its runtime representation with Double.
    void check_exp_equality_policies()
    {
        require(Runtime::Exp(Runtime::Let(
                    {Runtime::NonRec{Runtime::TrimmedExp({}, Runtime::Int(1))}},
                    Runtime::TrimmedExp({0}, Runtime::IndexVar(0)))) !=
                    Runtime::Exp(Runtime::Let(
                        {Runtime::Rec({Runtime::TrimmedExp({}, Runtime::Int(1))})},
                        Runtime::TrimmedExp({0}, Runtime::IndexVar(0)))),
                "Runtime equality should distinguish NonRec from Rec");

        const double nan = std::numeric_limits<double>::quiet_NaN();
        require(Runtime::Exp(Runtime::Double(nan)) == Runtime::Exp(Runtime::Double(nan)),
                "Runtime::Double NaNs should compare equal");
        auto log_nan = Runtime::Exp(exp_to_log_space(nan));
        require(log_nan.is_double(),
                "C++ log numbers should use the Runtime::Double representation");
        require(log_nan == Runtime::Exp(Runtime::Double(nan)),
                "C++ log-number NaNs should follow the Runtime::Double equality policy");
    }
}

// Runs the Runtime equality-policy tests.
void run_equality_tests()
{
    check_exp_equality_policies();
}
}
