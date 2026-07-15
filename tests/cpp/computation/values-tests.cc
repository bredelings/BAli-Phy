#include "computation/runtime/ast.H"
#include "test-util.H"
#include <limits>

namespace bali_phy_test
{
namespace
{
    // Checks binding-form distinctions and the deliberate Runtime NaN policy.
    void check_exp_equality_policies()
    {
        require(Runtime::Exp(Runtime::Let(
                    Runtime::NonRec{Runtime::Int(1)}, Runtime::IndexVar(0))) !=
                    Runtime::Exp(Runtime::Let(
                        Runtime::Rec({Runtime::Int(1)}), Runtime::IndexVar(0))),
                "Runtime equality should distinguish NonRec from Rec");

        const double nan = std::numeric_limits<double>::quiet_NaN();
        require(Runtime::Exp(Runtime::Double(nan)) == Runtime::Exp(Runtime::Double(nan)),
                "Runtime::Double NaNs should compare equal");
        require(Runtime::Exp(Runtime::LogDouble(log_double_t(nan))) ==
                    Runtime::Exp(Runtime::LogDouble(log_double_t(nan))),
                "Runtime::LogDouble NaNs should compare equal");
    }
}

// Runs the Runtime equality-policy tests.
void run_value_tests()
{
    check_exp_equality_policies();
}
}
