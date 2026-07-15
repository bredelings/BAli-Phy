#include "computation/closure.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/trim.H"
#include "test-util.H"

namespace bali_phy_test
{
namespace
{
    // Checks that applying a Runtime Trim selects the requested environment entries.
    void check_closure_trim()
    {
        auto C = get_trimmed(Runtime::Trim({0, 2}, Runtime::IndexVar(1)), {10, 20, 30});

        require(C.has_code(), "trimmed runtime closure should preserve Runtime::Exp code");
        auto body = C.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trimmed runtime closure should use the Runtime::Trim body");
        require(body->index == 1, "trimmed runtime closure body index mismatch");
        require((C.Env == closure::Env_t{10, 30}), "trimmed runtime closure environment mismatch");
    }

    // Checks that trim_unnormalize restores indices against the original environment.
    void check_closure_trim_unnormalize()
    {
        closure C(Runtime::Trim({0, 2}, Runtime::IndexVar(1)), {10, 20, 30});

        auto C2 = trim_unnormalize(C);

        require(C2.has_code(), "trim_unnormalize should preserve Runtime::Exp code");
        auto body = C2.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trim_unnormalize should remove the Runtime::Trim wrapper");
        require(body->index == 2, "trim_unnormalize should remap body indices through Trim indices");
        require((C2.Env == closure::Env_t{10, 20, 30}), "trim_unnormalize should preserve the closure environment");
    }

}

// Runs the direct closure representation and environment tests.
void run_closure_tests()
{
    check_closure_trim();
    check_closure_trim_unnormalize();
}
}
