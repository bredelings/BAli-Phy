#ifndef BUILTINS_NATIVE_VECTOR_VIEW_H
#define BUILTINS_NATIVE_VECTOR_VIEW_H

#include <cstddef>
#include <span>
#include <string_view>

#include "util/dense-matrix.H"
#include "util/myexception.H"

// Validate a signed native-vector slice once and expose its contiguous range.
// The returned span remains valid only while the vector owner remains alive.
template <typename T>
std::span<const T> checked_native_vector_view(const DenseVector<T>& owner,
                                              int offset, int count,
                                              std::string_view operation)
{
    if (offset < 0 or count < 0 or offset > owner.size() or
        count > owner.size() - offset)
        throw myexception()<<operation<<": invalid native vector view (offset "
                           <<offset<<", length "<<count<<", owner length "
                           <<owner.size()<<")";

    std::span<const T> storage(owner.data(),
                               static_cast<std::size_t>(owner.size()));
    return storage.subspan(static_cast<std::size_t>(offset),
                           static_cast<std::size_t>(count));
}

#endif
