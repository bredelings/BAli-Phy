#include "genetic_code.H"
#include "util/io.H"

using std::vector;
using std::string;
using std::istream;
using std::shared_ptr;

namespace fs = std::filesystem;

void Genetic_Code::add_entry(char c1, char c2, char c3, char aa)
{
    int n1 = dna[ c1 ];
    int n2 = dna[ c2 ];
    int n3 = dna[ c3 ];

    if (not dna.is_letter(n1)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c1<<" is not a nucleotide.";
    if (not dna.is_letter(n2)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c2<<" is not a nucleotide.";
    if (not dna.is_letter(n3)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c3<<" is not a nucleotide.";

    int aa_index = A[aa];

    if (not A.is_letter(aa_index)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<aa<<"' is not a valid amino acid.";

    translation_table[n1][n2][n3] = aa_index;
}

void Genetic_Code::setup_table(const string& n1, const string& n2, const string& n3, const string& aa)
{
    const int N = 64;
    assert(n1.size() == N);
    assert(n2.size() == N);
    assert(n3.size() == N);
    assert(aa.size() == N);

    translation_table = vector< vector< vector<int> > >(4,
							vector<vector<int> >(4,
									     vector<int>(4,-1)
							    )
	);

    for(int i=0;i<N;i++)
	add_entry(n1[i],n2[i],n3[i],aa[i]);

    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    for(int k=0;k<4;k++)
		if (translation_table[i][j][k] == -1)
		    throw myexception()<<"Codon "<<i<<","<<j<<","<<k<<" has no translation!";
}

void Genetic_Code::setup_table(const string& aa)
{
    string n1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG";
    string n2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG";
    string n3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG";
    setup_table(n1,n2,n3,aa);
}

void Genetic_Code::setup_table(std::istream& file)
{
    string aa;
    portable_getline(file,aa);

    string n1;
    portable_getline(file,n1);

    string n2;
    portable_getline(file,n2);

    string n3;
    portable_getline(file,n3);

    //---- Create the lookup table ----//
    setup_table(n1,n2,n3,aa);
}

Genetic_Code genetic_code_from_file(const std::string& name, const std::filesystem::path& filename)
{
    checked_ifstream file(filename,"genetic code file");
    return Genetic_Code(name,file);
}


int Genetic_Code::translate(int n1, int n2, int n3) const
{
    if (rna.is_feature(n1) and rna.is_feature(n2) and rna.is_feature(n3)) 
    {
	if (rna.is_letter(n1) and rna.is_letter(n2) and rna.is_letter(n3))
	{
	    int index = translation_table[n1][n2][n3];
	    if (index == -1) 
		throw myexception()<<"Genetic Code: "<<name()<<" has no entry for "<<n1<<","<<n2<<","<<n3;
	    return index;
	}
	else 
	    return alphabet::not_gap;
    }
    else if (n1 == alphabet::gap or n2 == alphabet::gap or n3 == alphabet::gap)
	return alphabet::gap;
    else
	return alphabet::unknown;
}

Genetic_Code::Genetic_Code(const string& n)
    :name_(n)
{
  
}

Genetic_Code::Genetic_Code(const string& n, istream& file)
    :name_(n)
{
    setup_table(file);
}

Standard_Genetic_Code::Standard_Genetic_Code()
    :Genetic_Code("standard")
{
    setup_table("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Invertebrate_Genetic_Code::Mt_Invertebrate_Genetic_Code()
    :Genetic_Code("mt-invert")
{
    setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG");
}

Mt_Vertebrate_Genetic_Code::Mt_Vertebrate_Genetic_Code()
    :Genetic_Code("mt-vert")
{
    setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG");
}

Mt_Yeast_Genetic_Code::Mt_Yeast_Genetic_Code()
    :Genetic_Code("mt-yeast")
{
    setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Protozoan_Genetic_Code::Mt_Protozoan_Genetic_Code()
    :Genetic_Code("mt-protozoan")
{
    setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

shared_ptr<const Genetic_Code> get_genetic_code(const string& name)
{
    if (name == "standard")
	return shared_ptr<const Genetic_Code>(new Standard_Genetic_Code());
    else if (name == "mt-vert")
	return shared_ptr<const Genetic_Code>(new Mt_Vertebrate_Genetic_Code());
    else if (name == "mt-invert")
	return shared_ptr<const Genetic_Code>(new Mt_Invertebrate_Genetic_Code());
    else if (name == "mt-yeast")
	return shared_ptr<const Genetic_Code>(new Mt_Yeast_Genetic_Code());
    else if (name == "mt-protozoan")
	return shared_ptr<const Genetic_Code>(new Mt_Protozoan_Genetic_Code());
    else
	throw myexception()<<"I don't recognize genetic code name '"<<name<<"'.\n"
	    "  Try one of 'standard', 'mt-vert', 'mt-invert', 'mt-protozoan', 'mt-yeast'.";
}

