#include "computation/loader.H"
#include "test-util.H"
#include <filesystem>
#include <memory>
#include <optional>
#include <vector>

namespace bali_phy_test
{
    void run_closure_tests();
    void run_equality_tests();
    void run_serialization_tests(const std::shared_ptr<module_loader>& loader);
    void run_transform_tests();
}

// Runs the computation tests with the package paths needed by component checks.
int main(int argc, char** argv)
{
    bali_phy_test::require(argc == 3, "expected builtin and package paths");

    auto loader = std::make_shared<module_loader>(
        std::optional<std::filesystem::path>{},
        std::vector<std::filesystem::path>{argv[1], argv[2]});

    bali_phy_test::run_serialization_tests(loader);
    bali_phy_test::run_closure_tests();
    bali_phy_test::run_equality_tests();
    bali_phy_test::run_transform_tests();
}
