#ifndef DP_SIZE_HH
#define DP_SIZE_HH

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <new>

// A required alignment table could not be built. The move can discard its candidate copies.
class dp_table_allocation_failure: public std::bad_alloc
{
public:
    const char* what() const noexcept override {return "could not allocate alignment DP table";}
};

// Check the byte size at allocation time so that signed offsets into the table need
// only ordinary multiplication and addition during the forward and backward passes.
template <typename T>
constexpr std::int64_t checked_dp_product(std::int64_t rows, std::int64_t stride)
{
    constexpr auto max_bytes = std::min<std::intmax_t>(std::numeric_limits<std::ptrdiff_t>::max(),
                                                       std::numeric_limits<std::int64_t>::max());
    constexpr std::int64_t max_elements = max_bytes / sizeof(T);
    if (rows < 0 or stride < 0 or (stride > 0 and rows > max_elements / stride))
        throw std::bad_array_new_length();
    return rows * stride;
}

#endif
