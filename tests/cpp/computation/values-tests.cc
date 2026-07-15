#include "computation/runtime/ast.H"
#include "test-util.H"
#include <limits>

namespace bali_phy_test
{
namespace
{
    // Checks conversion of Runtime vectors to their corresponding C++ vectors.
    void check_vector_conversions()
    {
        Runtime::RVector ints(std::vector<int>{1, 2, 3});
        require(((std::vector<int>)ints == std::vector<int>{1, 2, 3}),
                "Runtime::RVector int conversion mismatch");

        Runtime::RVector chars(std::vector<char32_t>{U'a', U'b'});
        require(((std::vector<char32_t>)chars == std::vector<char32_t>{U'a', U'b'}),
                "Runtime::RVector char conversion mismatch");
    }

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

// Runs the remaining Runtime conversion and equality-policy tests.
void run_value_tests()
{
    check_vector_conversions();
    check_exp_equality_policies();
}
}
