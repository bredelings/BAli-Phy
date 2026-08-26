#include "alignment/alignment.hh"
#include "sequence/ambiguity.hh"
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

// Protect virtual ambiguity notation, data-local code reuse, and Cartesian-hull
// detection; ordinary likelihood tests cannot construct arbitrary masks. This is
// obsolete when existing input/output tests cover these bidirectional invariants.
int main()
{
    DNA dna;
    const std::vector<std::pair<std::string, unsigned long>> iupac = {
        {"Y", 0b1010}, {"R", 0b0101}, {"W", 0b1001}, {"S", 0b0110}, {"K", 0b1100},
        {"M", 0b0011}, {"B", 0b1110}, {"D", 0b1101}, {"H", 0b1011}, {"V", 0b0111}
    };
    for (const auto& [symbol, bits]: iupac)
    {
        auto mask = dna.mask_for_symbol(symbol);
        require(mask and mask->to_ulong() == bits, "DNA did not recognize IUPAC symbol " + symbol + ".");
        require(dna.lookup(*mask) == std::pair(symbol, true),
                "DNA did not write IUPAC symbol " + symbol + " exactly.");
    }

    RNA rna;
    auto rna_y = rna.mask_for_symbol("Y");
    require(rna_y and (*rna_y)[rna.C()] and (*rna_y)[rna.T()], "RNA Y did not contain C and U.");
    require(rna.mask_for_symbol("U") == alphabet::bitmask_t(4, 0b1000), "RNA did not recognize exact U.");
    require(not dna.mask_for_symbol("!") and not dna.mask_for_symbol("-") and not dna.mask_for_symbol("?"),
            "Uninterpretable or special nucleotide symbols were reported as state masks.");

    ambiguity_database nucleotide_ambiguities(dna.n_letters());
    bool rejected = false;
    try
    {
        nucleotide_ambiguities.encode_symbol(dna, "!");
    }
    catch (const bad_letter&)
    {
        rejected = true;
    }
    require(rejected, "Public ambiguity encoding did not reject an uninterpretable symbol.");

    AminoAcids amino_acids;
    for (const std::string symbol: {"B", "Z", "J"})
    {
        auto mask = amino_acids.mask_for_symbol(symbol);
        require(mask and amino_acids.lookup(*mask) == std::pair(symbol, true),
                "Amino-acid ambiguity " + symbol + " did not round-trip exactly.");
    }

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
    non_product.set(triplets.find_letter("AAA"));
    non_product.set(triplets.find_letter("CCC"));
    int non_product_code = data.get_ambiguities().encode_mask(non_product);
    auto widened = data.decode(non_product_code);
    require(widened.first == "MMM" and not widened.second,
            "A non-product ambiguity did not widen to its Cartesian hull.");
    require(data.consistent(non_product_code, triplets.find_letter("AAA")),
            "An arbitrary ambiguity was not consistent with one of its states.");
    require(not data.consistent(non_product_code, triplets.find_letter("GGG")),
            "An arbitrary ambiguity was consistent with a state outside its mask.");
}
