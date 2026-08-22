#pragma once

#include <stdexcept>
#include <string>

namespace bali_phy_test
{
    // Reports a failed test condition without depending on assertions being enabled.
    inline void require(bool condition, const std::string& message)
    {
        if (not condition)
            throw std::runtime_error(message);
    }
}

#define BALI_PHY_TEST_CHECK(condition) \
    ::bali_phy_test::require(bool(condition), #condition)
