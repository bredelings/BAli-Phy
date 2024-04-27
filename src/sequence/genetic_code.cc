#include "genetic_code.H"
#include "util/io.H"
#include "util/string/convert.H"
#include "util/string/join.H"

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
    return Genetic_Code(name, {}, file);
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

Genetic_Code::Genetic_Code(const string& n, std::optional<int> num)
    :name_(n), number_(num)
{
  
}

Genetic_Code::Genetic_Code(const string& n, std::optional<int> num, istream& file)
    :name_(n), number_(num)
{
    setup_table(file);
}

Genetic_Code::Genetic_Code(const std::string& name, std::optional<int> num, const std::string& code)
    :name_(name), number_(num)
{
    setup_table(code);
}

Genetic_Code::Genetic_Code(const std::string& name, std::optional<int> num, const std::string& n1, const std::string& n2, const std::string& n3, const std::string& code)
    :name_(name), number_(num)
{
    setup_table(n1,n2,n3,code);
}

std::vector< Genetic_Code> genetic_codes = {
    {"standard", 1, "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Standard
    {"mt-vert", 2, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG"}, // Mt: Vertebrate
    {"mt-yeast", 3, "FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Mt: Yeast
    // *: Mold, Protozoan, and Coelenterate Mitochondrial Code and the Mycoplasma/Spiroplasma
    {"mt-protozoa", 4, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"},
    {"mt-invert", 5, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG"}, // Mt: Invertebrate
    {"nuc-ciliate", 6, "FFLLSSSSYYQQCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc: Ciliate, Dasycladacean and Hexamita
    // no 7-8
    {"mt-echinoderm", 9, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG"}, // Mt:  Echinoderm and Flatworm
    {"nuc-euplotid", 10, "FFLLSSSSYY**CCCWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc: Euplotid
    {"bacteria", 11, "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // *:   Bacterial, Archaeal and Plant Plastid
    {"nuc-yeast-alt", 12, "FFLLSSSSYY**CC*WLLLSPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc: Alternative Yeast
    {"mt-ascidian", 13, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSGGVVVVAAAADDEEGGGG"}, // Mt:  Ascidian
    {"mt-flatworm-alt", 14, "FFLLSSSSYYY*CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG"}, // Mt:  Alternative Flatworm
    {"nuc-blepharisma", 15, "FFLLSSSSYY*QCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc: Blepharisma Nuclear Code
    {"mt-chlorophycean", 16, "FFLLSSSSYY*LCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Mt:  Chlorophycean
    // no 17-20
    {"mt-trematode", 21, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNNKSSSSVVVVAAAADDEEGGGG"}, // Mt:   Trematode
    {"mt-scenedesmus-obliquus", 22, "FFLLSS*SYY*LCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Mt:   Scenedesmus obliquus
    {"mt-thraustochytrium", 23, "FF*LSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Mt:   Thraustochytrium
    {"mt-rhabdopleuridae", 24, "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSSKVVVVAAAADDEEGGGG"}, // Mt:   Rhabdopleuridae
    {"bacteria-sr1", 25, "FFLLSSSSYY**CCGWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // *:    Candidate Division SR1 and Gracilibacteria
    {"nuc-pachysolen-tannophilus", 26, "FFLLSSSSYY**CC*WLLLAPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Pachysolen tannophilus
    {"nuc-karyorelict", 27, "FFLLSSSSYYQQCCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Karyorelict
    {"nuc-condylostoma", 28, "FFLLSSSSYYQQCCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Condylostoma 
    {"nuc-mesodinium", 29, "FFLLSSSSYYYYCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Mesodinium
    {"nuc-peritrich", 30, "FFLLSSSSYYEECC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Peritrich
    {"nuc-blastocrithidia", 31, "FFLLSSSSYYEECCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"}, // Nuc:  Blastocrithidia
    // no 32
    {"mt-cephalodiscidae", 33, "FFLLSSSSYYY*CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSSKVVVVAAAADDEEGGGG"}, // Mt:   Cephalodiscidae UAA-Tyr
};

Genetic_Code get_genetic_code(const string& name)
{
    if (name.starts_with("code"))
    {
	string rest = name.substr(4);
	if (auto number = can_be_converted_to<int>(rest))
	    return get_genetic_code(*number);
	else
	    throw myexception()<<"Can't find genetic code name '"<<name<<"' because suffix '"<<rest<<" is not an integer.";
    }
	
    for(auto& code: genetic_codes)
	if (code.name() == name)
	    return code;

    vector<string> names;
    for(auto& code: genetic_codes)
	names.push_back(code.name());
    auto e = myexception()<<"I don't recognize genetic code name '"<<name<<"'.\n"<<"  Valid names are code<number>, "<<join(names,", ")<<".\n  See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi for more information.";

    throw e;
}

Genetic_Code get_genetic_code(int number)
{
    for(auto& code: genetic_codes)
	if (code.number() == number)
	    return code;

    throw myexception()<<"I don't recognize genetic code number "<<number<<".\n  Valid numbers are 1-6,9-16,21-31,33.\n  See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi for more information.";
}

Genetic_Code standard_code() {return get_genetic_code(1);}
