#include "computation/runtime/ast.H"
#include "test-util.H"
#include <limits>

namespace bali_phy_test
{
namespace
{
    // Checks atomic Runtime value classification and typed accessors.
    void check_atomic_values()
    {
        Runtime::Exp i = 7;
        require(i.is_atomic_value(), "runtime Int should be atomic");
        require(i.as_int() == 7, "runtime Int value mismatch");

        Runtime::Exp d = 2.5;
        require(d.is_atomic_value(), "runtime Double should be atomic");
        require(d.as_double() == 2.5, "runtime Double value mismatch");

        auto ld_value = log_double_t(0.25);
        Runtime::Exp ld = ld_value;
        require(ld.is_atomic_value(), "runtime LogDouble should be atomic");
        require(ld.as_log_double() == ld_value, "runtime LogDouble value mismatch");

        Runtime::Exp c = U'x';
        require(c.is_atomic_value(), "runtime Char should be atomic");
        require(c.as_char() == U'x', "runtime Char value mismatch");

        Runtime::Exp s = std::string("runtime");
        require(s.is_atomic_value(), "runtime String should be atomic");
        require(s.as_string() == "runtime", "runtime String value mismatch");

        Runtime::Exp n = integer("12345678901234567890");
        require(n.is_atomic_value(), "runtime Integer should be atomic");
        require(n.as_integer() == integer("12345678901234567890"),
                "runtime Integer value mismatch");

        Runtime::Exp b = Runtime::ConstructorApp("Data.Bool.True", 0, {});
        require(b.is_atomic_value(), "nullary runtime ConstructorApp should be atomic");
        require(b.as_constructor().name() == "Data.Bool.True",
                "runtime ConstructorApp name mismatch");
        require(b.as_constructor().n_args() == 0, "runtime ConstructorApp arity mismatch");

        Runtime::Exp vector_value = R::RVector(std::vector<int>{1, 2});
        require(vector_value.is_atomic_value(), "runtime ObjectValue should be atomic");
        require(vector_value.as_<R::RVector>().size() == 2,
                "runtime ObjectValue value mismatch");

        Runtime::Exp pair = Runtime::ConstructorApp(
            "Pair", 2, {Runtime::Exp(1), Runtime::Exp("field")});
        require(pair.is_value(), "constructor applications should be runtime values");
        auto app = pair.to<Runtime::ConstructorApp>();
        require(bool(app), "constructor application should convert to Runtime::ConstructorApp");
        require(app->args.size() == 2, "constructor application arity mismatch");
        require(app->args[0].as_int() == 1, "constructor application int field mismatch");
        require(app->args[1].as_string() == "field",
                "constructor application string field mismatch");
    }

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

    // Checks structural equality, binding-form distinctions, and NaN equality policy.
    void check_exp_equality()
    {
        Runtime::Exp e1 = Runtime::Let(
            Runtime::Rec({Runtime::Int(1), Runtime::String("field")}),
            Runtime::ConstructorApp(
                "Pair", 2, {Runtime::IndexVar(0), Runtime::IndexVar(1)}));
        Runtime::Exp e2 = Runtime::Let(
            Runtime::Rec({Runtime::Int(1), Runtime::String("field")}),
            Runtime::ConstructorApp(
                "Pair", 2, {Runtime::IndexVar(0), Runtime::IndexVar(1)}));
        Runtime::Exp e3 = Runtime::Let(
            Runtime::Rec({Runtime::Int(2), Runtime::String("field")}),
            Runtime::ConstructorApp(
                "Pair", 2, {Runtime::IndexVar(0), Runtime::IndexVar(1)}));

        require(e1 == e2, "matching Runtime::Exp trees should compare equal");
        require(not (e1 == e3), "different Runtime::Exp trees should not compare equal");
        require(Runtime::Exp(Runtime::Let(
                    Runtime::NonRec{Runtime::Int(1)}, Runtime::IndexVar(0))) !=
                    Runtime::Exp(Runtime::Let(
                        Runtime::Rec({Runtime::Int(1)}), Runtime::IndexVar(0))),
                "Runtime equality should distinguish NonRec from Rec");
        require(not (Runtime::ConstructorApp("Pair", 2, {}) ==
                     Runtime::ConstructorApp("Pair", 1, {})),
                "Runtime constructor application equality should include arity");

        const double nan = std::numeric_limits<double>::quiet_NaN();
        require(Runtime::Exp(Runtime::Double(nan)) == Runtime::Exp(Runtime::Double(nan)),
                "Runtime::Double NaNs should compare equal");
        require(Runtime::Exp(Runtime::LogDouble(log_double_t(nan))) ==
                    Runtime::Exp(Runtime::LogDouble(log_double_t(nan))),
                "Runtime::LogDouble NaNs should compare equal");
    }
}

// Runs the Runtime value representation and equality tests.
void run_value_tests()
{
    check_atomic_values();
    check_vector_conversions();
    check_exp_equality();
}
}
