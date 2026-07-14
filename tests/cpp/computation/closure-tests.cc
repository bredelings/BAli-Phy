#include "computation/machine/graph_register.H"
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

    // Checks resolution of function, reference, and constructor slots through closure environments.
    void check_closure_slots()
    {
        closure C(Runtime::FunctionApp(Runtime::IndexVar(1),
                                       {Runtime::IndexVar(0), Runtime::RegRef(30)}),
                  {10, 20});

        require(C.n_function_args() == 2, "runtime closure function argument count mismatch");
        require(C.reg_for_code(C.function_head_ref()) == 10,
                "runtime function head should resolve through the closure environment");
        require(C.reg_for_function_arg(0) == 20,
                "runtime function argument should resolve through the closure environment");
        require(C.reg_for_function_arg(1) == 30,
                "runtime RegRef function argument should preserve its target");

        require(closure(Runtime::IndexVar(1), {10, 20}).reg_for_ref_maybe() == 10,
                "runtime closure IndexVar ref should resolve through the closure environment");
        require(closure(Runtime::RegRef(30), {10, 20}).reg_for_ref_maybe() == 30,
                "runtime closure RegRef should preserve its target");
        require(not closure(Runtime::Int(1)).reg_for_ref_maybe(),
                "non-reference runtime closure should not have a referenced reg");

        closure C3(Runtime::ConstructorApp("Test.Pair", 2,
                                           {Runtime::IndexVar(1), Runtime::RegRef(30)}),
                   {10, 20});
        auto slot = C3.constructor_slot(0);
        auto slot_ref = slot.to<Runtime::RegRef>();
        require(bool(slot_ref), "runtime IndexVar constructor slot should become a RegRef");
        require(slot_ref->target == 10, "runtime constructor slot RegRef target mismatch");
    }
}

// Runs the direct closure representation and environment tests.
void run_closure_tests()
{
    check_closure_trim();
    check_closure_trim_unnormalize();
    check_closure_slots();
}
}
