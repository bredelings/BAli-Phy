#include "triplets.hh"
#include "codons.hh"

using std::vector;
using std::string;
using std::valarray;

void Triplets::setup_sub_nuc_table()
{
    triplet_for_components = vector<vector<vector<int>>>(
        N->n_letters(), vector<vector<int>>(N->n_letters(), vector<int>(N->n_letters(), -1)));
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
	triplet_for_components[n0][n1][n2] = i;
    }
}

// Map three exact nucleotide states directly to their exact triplet state.
int Triplets::get_triplet(int n1, int n2, int n3) const
{
    if (not N->is_letter(n1) or not N->is_letter(n2) or not N->is_letter(n3))
        throw myexception()<<"get_triplet requires three exact nucleotide states.";
    int state = triplet_for_components[n1][n2][n3];
    if (state == -1)
        throw myexception()<<"The nucleotide triple is not in this triplet alphabet.";
    return state;
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

valarray<double> Triplets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_codon_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

// Render the positional projections, then compare the states represented by
// their product with the original set to detect lossy Cartesian widening.
std::pair<string, bool> Triplets::lookup(const bitmask_t& mask) const
{
    auto [spelling, exact] = alphabet::lookup(mask);
    if (exact)
        return {spelling, true};

    vector<bitmask_t> projections(3, bitmask_t(N->n_letters()));
    for (auto state = mask.find_first(); state != bitmask_t::npos; state = mask.find_next(state))
        for (int pos = 0; pos < 3; pos++)
            projections[pos].set(sub_nuc(state, pos));

    spelling.clear();
    for (auto& projection: projections)
    {
        auto [component_spelling, component_exact] = N->lookup(projection);
        spelling += component_spelling;
        if (not component_exact)
            projection.set();
    }

    bitmask_t hull(n_letters());
    for (int state = 0; state < n_letters(); state++)
        if (projections[0][sub_nuc(state, 0)] and projections[1][sub_nuc(state, 1)] and
            projections[2][sub_nuc(state, 2)])
            hull.set(state);
    return {spelling, hull == mask};
}

// Reproduce mixed-gap, length, and stop-codon diagnostics only on parse errors.
void Triplets::validate_sequence(const string& letters) const
{
    const int letter_size = width();
    vector<int> singlets(letters.size());
    for (int i = 0; i < singlets.size(); i++)
        singlets[i] = (*N)[letters.substr(i, 1)];

    int n_letters = 0;
    for (auto singlet: singlets)
        if (is_feature(singlet))
            n_letters++;

    myexception e;
    bool ok = true;
    vector<int> stop_codons;
    for (int i = 0; i < singlets.size() / letter_size and ok; i++)
    {
        int l1 = singlets[letter_size * i];
        int l2 = singlets[letter_size * i + 1];
        int l3 = singlets[letter_size * i + 2];
        if (is_feature(l1) and is_feature(l2) and is_feature(l3))
        {
            if (N->is_letter(l1) and N->is_letter(l2) and N->is_letter(l3) and
                triplet_for_components[l1][l2][l3] == -1)
                stop_codons.push_back(i);
        }
        else if (l1 == gap and l2 == gap and l3 == gap)
            continue;
        else
        {
            e<<" Sequence not aligned as "<<letters_name()<<"!  Column "<<i+1
             <<" has mixed gap/non-gap letter '"<<letters.substr(i * letter_size, letter_size)<<"'";
            ok = false;
        }
    }
    if (n_letters % letter_size != 0)
    {
        if (not ok) e<<"\n";
        e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "
         <<letters_name()<<": not a multiple of 3 "<<N->letters_name()<<"!";
        ok = false;
    }
    if (singlets.size() % letter_size != 0)
    {
        if (not ok) e<<"\n";
        e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "
         <<letters_name()<<": not a multiple of 3 columns!";
        ok = false;
    }
    if (not stop_codons.empty())
    {
        if (not ok) e<<"\n";
        ok = false;
        auto codons = dynamic_cast<const Codons*>(this);
        assert(codons);
        e<<" Sequence contains "<<stop_codons.size()<<" stop codons: not allowed!\n";
        int column = stop_codons.front();
        e<<"   First stop codon is '"<<letters.substr(column * letter_size, letter_size)
         <<"' at nucleotide column "<<3 * column + 1
         <<"   (genetic code = "<<codons->getGenetic_Code().name()<<")";
    }
    if (not ok)
        throw e;
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
    :Triplets(string("Triplets(")+a.name+")",a)
{ }
