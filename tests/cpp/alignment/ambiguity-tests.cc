#include "alignment/alignment.hh"
#include "sequence/triplets.hh"

#include <cstdlib>
#include <iostream>
#include <string>

namespace
{

/// Terminate with a concise diagnostic when an ambiguity invariant is violated.
void require(bool condition, const std::string& message)
{
    if (condition)
        return;
    std::cerr<<message<<"\n";
    std::exit(1);
}

/// Construct a named sequence whose inherited string contains the supplied symbols.
sequence make_sequence(const std::string& name, const std::string& symbols)
{
    sequence result;
    result.name = name;
    result.assign(symbols);
    return result;
}

}

// Protect data-local code reuse, exact product spelling, and lossy Cartesian-hull
// detection; ordinary likelihood tests cannot construct arbitrary masks. This is
// obsolete when an existing sequence test or input format covers non-Cartesian sets.
int main()
{
    DNA dna;
    Triplets triplets(dna);
    alignment data(triplets);
    data.load({make_sequence("first", "RAY"), make_sequence("second", "RAY")});

    int observed_code = data(0, 0);
    require(observed_code <= alphabet::first_ambiguity, "RAY was not encoded as a data-local ambiguity.");
    require(data(0, 1) == observed_code, "Equal observed masks did not reuse one ambiguity code.");
    require(data.get_ambiguities().size() == 1, "The database stored more than the observed proper mask.");

    auto observed = data.decode(observed_code);
    require(observed.first == "RAY" and observed.second,
            "A Cartesian-product ambiguity did not round-trip exactly.");

    alphabet::bitmask_t non_product(triplets.n_letters());
    non_product.set(triplets["AAA"]);
    non_product.set(triplets["CCC"]);
    int non_product_code = data.get_ambiguities().encode_mask(non_product);
    auto widened = data.decode(non_product_code);
    require(widened.first == "MMM" and not widened.second,
            "A non-product ambiguity did not widen to its Cartesian hull.");
    require(data.consistent(non_product_code, triplets["AAA"]),
            "An arbitrary ambiguity was not consistent with one of its states.");
    require(not data.consistent(non_product_code, triplets["GGG"]),
            "An arbitrary ambiguity was consistent with a state outside its mask.");
}
