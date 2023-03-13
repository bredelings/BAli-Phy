#include "triplets.H"
#include "codons.H"

using std::vector;
using std::string;
using std::valarray;

void Triplets::setup_sub_nuc_table()
{
    codon_table = vector<vector<vector<int>>>(4,vector<vector<int> >(4,vector<int>(4,-1)));

    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++) {
	const string& codon = letter(i);

	assert(codon.length() == 3);
	sub_nuc_table[i].resize(3);

	int n0 = sub_nuc_table[i][0] = (*N)[ codon.substr(0,1) ];
	int n1 = sub_nuc_table[i][1] = (*N)[ codon.substr(1,1) ];
	int n2 = sub_nuc_table[i][2] = (*N)[ codon.substr(2,1) ];

	codon_table[n0][n1][n2] = i;
    }
}

int Triplets::sub_nuc(int codon,int pos) const {
    assert( 0 <= pos and pos <= 3);

    return sub_nuc_table[codon][pos];
}

vector<string> getTriplets(const vector<string>& v) 
{
    vector<string> w;
    for(int i=0;i<v.size();i++) {
	string s1 = v[i];
	for(int j=0;j<v.size();j++) {
	    string s2 = s1 + v[j];
	    for(int k=0;k<v.size();k++) {
		string s3 = s2 + v[k];
		w.push_back(s3);
	    }
	}
    }
    return w;
}

vector<string> getTriplets(const Nucleotides& a) {
    vector<string> v;
    for(int i=0;i<a.size();i++)
	v.push_back(a.lookup(i));
    return getTriplets(v);
}


bool matches(const string& c1,const string& c2,const Nucleotides& N)
{
    assert(c1.size() == 3);
    assert(c1.size() == c2.size());

    for(int n=0;n<3;n++) {
	string l1 = c1.substr(n,1);
	string l2 = c2.substr(n,1);

	int i1 = N.find_letter(l1);
	int i2 = N[l2];

	if (not N.matches(i1,i2))
	    return false;
    }
    return true;
}

// alphabet: already set
// unknown_letters: already set
void Triplets::setup_letter_classes() 
{
    // clear masks and classes to just the letters
    alphabet::setup_letter_classes();

    // get nucleotide letters
    vector<string> v = N->letter_classes();
    v.push_back(N->wildcard);

    // construct letter classes names
    vector<string> w = getTriplets(v);
  
    // construct letter class masks
    bitmask_t empty_mask(size());
    bitmask_t mask(size());

    for(int i=0;i<w.size();i++) {
	if (contains(w[i])) continue;
	if (w[i] == wildcard) continue;

	mask = empty_mask;

	bool found = false;
	for(int j=0;j<mask.size();j++) {
	    if (::matches(letter(j),w[i],*N)) {
		mask[j] = true;
		found = true;
	    }
	}
	if (found)
	    insert_class(w[i],mask);
    }
}

valarray<double> get_nucleotide_counts_from_codon_counts(const Triplets& C,const valarray<double>& C_counts) {
    const Nucleotides& N = C.getNucleotides();

    valarray<double> N_counts(0.0,N.size());
    // For each codon type
    for(int i=0;i<C.size();i++) {
	// For each position in the codon
	for(int pos=0;pos<3;pos++)
	    // Count the nucleotides that occur there
	    N_counts[ C.sub_nuc(i,pos) ] += C_counts[i];
    }

    return N_counts;
}

valarray<double> get_codon_frequencies_from_independent_nucleotide_frequencies(const Triplets& C,const valarray<double>& fN ) {
    valarray<double> fC(C.size());
    for(int i=0;i<fC.size();i++) {
	fC[i] = 1.0;
	for(int pos=0;pos<3;pos++)
	    fC[i] *= fN[ C.sub_nuc(i,pos) ];
    }

    fC /= fC.sum();
    return fC;
}

int Triplets::get_triplet(int n1, int n2, int n3) const
{
    if (N->is_feature(n1) and N->is_feature(n2) and N->is_feature(n3)) 
    {
	if (N->is_letter(n1) and N->is_letter(n2) and N->is_letter(n3))
	{
	    int index = codon_table[n1][n2][n3];
	    if (index == -1) 
		throw myexception()<<"get_triplet: Triplet is not in this alphabet";
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

valarray<double> Triplets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_codon_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

vector<int> Triplets::operator()(const string& letters) const
{
    const int letter_size = width();

    myexception e;

    // 1. First translate singlets.
    vector<int> singlets = (*N)(letters);

    // FIXME -- if ? gets here from a user who means N/X then things will get confusing fast.

    // 2. Second count the number of non-gap letters.
    int n_letters = 0;
    for(auto s: singlets)
	if (is_feature(s)) n_letters++;

    // 3. Try to load alignment row with gaps
    bool ok = true;
    vector<int> triplets(singlets.size()/letter_size);

    vector<int> stop_codons;
    for(int i=0;i<triplets.size() and ok;i++)
    {
	int l1 = singlets[letter_size * i + 0];
	int l2 = singlets[letter_size * i + 1];
	int l3 = singlets[letter_size * i + 2];

	if (is_feature(l1) and is_feature(l2) and is_feature(l3))
	{
	    try {
		triplets[i] = get_triplet(l1, l2, l3);
	    }
	    catch (...)
	    {
		stop_codons.push_back(i);
	    }
	}

	else if (l1 == alphabet::gap and l2 == alphabet::gap and l3 == alphabet::gap)
	    triplets[i] = alphabet::gap;
	else
	{
	    e<<" Sequence not aligned as "<<letters_name()<<"!  Column "<<i+1<<" has mixed gap/non-gap letter '"<<letters.substr(i*letter_size,letter_size)<<"'";
	    ok = false;
	}
    }

    // 4. Check if we have the right number of letters.
    if (n_letters%letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "<<letters_name() <<": not a multiple of 3 "<<N->letters_name()<<"!";
	ok = false;
    }

    // 5. Check for extra columns we haven't been using
    if (singlets.size() % letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "<<letters_name() <<": not a multiple of 3 columns!";
	ok = false;
    }

    if (stop_codons.size())
    {
	if (not ok) e<<"\n";
	ok = false;
	auto C = dynamic_cast<const Codons*>(this);
	e<<" Sequence contains "<<stop_codons.size()<<" stop codons: not allowed!\n";

	int col = stop_codons[0];
	string stop = letters.substr(col*letter_size, letter_size);
	e<<"   First stop codon is '"<<stop<<"' at nucleotide column "<<3*col+1<<"   (genetic code = "<<C->getGenetic_Code().name()<<")";
    }

    if (not ok)
	throw e;

    return triplets;
}

Triplets::Triplets(const string& s,const Nucleotides& a)
    :alphabet(s,getTriplets(a)),N(a)
{
    // compute our 'wildcard' letter
    wildcard = N->wildcard+N->wildcard+N->wildcard;

    // compute our 'gap' letters
    gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

    // compute our 'unknown' letters
    unknown_letters.clear();
    for(auto& unknown_letter: N->unknown_letters)
        unknown_letters.push_back( unknown_letter + unknown_letter + unknown_letter);

    setup_sub_nuc_table();

    setup_letter_classes();
}

Triplets::Triplets(const Nucleotides& a)
    :Triplets(string("Triplets[")+a.name+"]",a)
{ }

