#ifndef COMPONENT_STATE_H
#define COMPONENT_STATE_H

#include "util/dense-matrix.hh"

// Own the structure-of-arrays result produced by component-state samplers.
struct ComponentStateVectors
{
    DenseVector<int> components;
    DenseVector<int> states;

    explicit ComponentStateVectors(int count)
        : components(count), states(count) {}
};

#endif
