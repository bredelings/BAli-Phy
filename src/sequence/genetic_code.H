#ifndef GENETIC_CODE_H
#define GENETIC_CODE_H

#include <vector>
#include <string>
#include <memory>
#include <optional>

#include "alphabet.H"

/// A Class that maps 3 nucleotides to an outcome in the amino_acid+stop alphabet
class Genetic_Code
{
    std::string name_;

    // From https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
    std::optional<int> number_;

    RNA rna;
    DNA dna;
    AminoAcidsWithStop A;

    std::vector< std::vector< std::vector<int> > > translation_table;

    void add_entry(char c1, char c2, char c3, char aa);

protected:
    void setup_table(const std::string&, const std::string&, const std::string&, const std::string&);

    void setup_table(const std::string&);

    void setup_table(std::istream&);

public:
    virtual Genetic_Code* clone() const {return new Genetic_Code(*this);}

    const std::string& name() const {return name_;}

    std::optional<int> number() const {return number_;}

    const RNA& get_RNA() const {return rna;}
    const DNA& get_DNA() const {return dna;}

    const AminoAcidsWithStop& get_amino_acids() const {return A;}

    /// Find out which amino acid (or stop codon) the letter maps to.
    int translate(int,int,int) const;

    /// Is letter codon a stop codon?
    bool is_stop_codon(int, int, int) const;

    Genetic_Code& operator=(const Genetic_Code&) = default;

    Genetic_Code& operator=(Genetic_Code&&) noexcept = default;

    Genetic_Code(const std::string& name, std::optional<int>);

    Genetic_Code(const std::string& name, std::optional<int>, std::istream&);

    Genetic_Code(const std::string& name, std::optional<int>, const std::string& code);

    Genetic_Code(const std::string& name, std::optional<int>, const std::string& n1, const std::string& n2, const std::string& n3, const std::string& code);

    Genetic_Code(const Genetic_Code&) = default;

    Genetic_Code(Genetic_Code&&) noexcept = default;

    virtual ~Genetic_Code() = default;
};

// From https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
Genetic_Code get_genetic_code(const std::string& name);
Genetic_Code get_genetic_code(int number);

Genetic_Code get_genetic_code_from_file(const std::string& name, const std::filesystem::path& filename);

Genetic_Code standard_code();

#endif
