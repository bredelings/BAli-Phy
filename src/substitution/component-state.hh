#ifndef COMPONENT_STATE_H
#define COMPONENT_STATE_H

#include <cstdint>
#include "util/dense-matrix.hh"

// Own the structure-of-arrays result produced by component-state samplers.
struct ComponentStateVectors
{
    DenseVector<std::int64_t> components;
    DenseVector<std::int64_t> states;

    explicit ComponentStateVectors(int count)
        : components(count), states(count) {}
};

#endif
