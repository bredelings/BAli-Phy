#include "computation/closure.H"
#include "computation/runtime/ast.H"
#include "test-util.H"

namespace bali_phy_test
{
namespace
{
    // Checks that applying a structural trim selects the requested environment entries.
    void check_closure_trim()
    {
        auto C = get_trimmed(Runtime::TrimmedExp({0, 2}, Runtime::IndexVar(1)),
                             {10, 20, 30});

        require(C.has_code(), "trimmed runtime closure should preserve Runtime::Exp code");
        auto body = C.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trimmed runtime closure should use the structural trim body");
        require(body->index == 1, "trimmed runtime closure body index mismatch");
        require((C.Env == closure::Env_t{10, 30}), "trimmed runtime closure environment mismatch");
    }

}

// Runs the direct closure representation and environment tests.
void run_closure_tests()
{
    check_closure_trim();
}
}
