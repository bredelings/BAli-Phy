#ifndef GENETIC_CODE_H
#define GENETIC_CODE_H

#include <vector>
#include <string>
#include <memory>

#include "alphabet.H"

/// A Class that maps 3 nucleotides to an outcome in the amino_acid+stop alphabet
class Genetic_Code
{
    std::string name_;

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

    const RNA& get_RNA() const {return rna;}
    const DNA& get_DNA() const {return dna;}

    const AminoAcidsWithStop& get_amino_acids() const {return A;}

    /// Find out which amino acid (or stop codon) the letter maps to.
    int translate(int,int,int) const;

    /// Is letter codon a stop codon?
    bool is_stop_codon(int, int, int) const;

    Genetic_Code(const std::string& name);

    Genetic_Code(const std::string& name, std::istream&);

    virtual ~Genetic_Code() = default;
};

Genetic_Code read_code_from_file(const std::string& name, const std::filesystem::path& filename);


class Standard_Genetic_Code: public Genetic_Code
{
public:
    Standard_Genetic_Code* clone() const {return new Standard_Genetic_Code(*this);}

    Standard_Genetic_Code();
};

class Mt_Invertebrate_Genetic_Code: public Genetic_Code
{
public:
    Mt_Invertebrate_Genetic_Code* clone() const {return new Mt_Invertebrate_Genetic_Code(*this);}

    Mt_Invertebrate_Genetic_Code();
};

class Mt_Vertebrate_Genetic_Code: public Genetic_Code
{
public:
    Mt_Vertebrate_Genetic_Code* clone() const {return new Mt_Vertebrate_Genetic_Code(*this);}

    Mt_Vertebrate_Genetic_Code();
};

class Mt_Protozoan_Genetic_Code: public Genetic_Code
{
public:
    Mt_Protozoan_Genetic_Code* clone() const {return new Mt_Protozoan_Genetic_Code(*this);}

    Mt_Protozoan_Genetic_Code();
};

class Mt_Yeast_Genetic_Code: public Genetic_Code
{
public:
    Mt_Yeast_Genetic_Code* clone() const {return new Mt_Yeast_Genetic_Code(*this);}

    Mt_Yeast_Genetic_Code();
};

class Mt_Echinoderm_Genetic_Code: public Genetic_Code
{
public:
    Mt_Echinoderm_Genetic_Code* clone() const {return new Mt_Echinoderm_Genetic_Code(*this);}

    Mt_Echinoderm_Genetic_Code();
};


std::shared_ptr<const Genetic_Code> get_genetic_code(const std::string& name);
#endif
