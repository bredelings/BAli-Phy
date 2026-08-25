#include "doublets.hh"

using std::vector;
using std::string;
using std::valarray;

void Doublets::setup_sub_nuc_table()
{
    doublet_for_components = vector<vector<int>>(N->n_letters(), vector<int>(N->n_letters(), -1));
    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++)
    {
	auto& doublet = letter(i);

	assert(doublet.length() == 2);
	sub_nuc_table[i].resize(2);

	int n0 = sub_nuc_table[i][0] = (*N)[ doublet.substr(0,1) ];
	int n1 = sub_nuc_table[i][1] = (*N)[ doublet.substr(1,1) ];
	doublet_for_components[n0][n1] = i;
    }
}

// Map two exact nucleotide states directly to their exact doublet state.
int Doublets::get_doublet(int n1, int n2) const
{
    if (not N->is_letter(n1) or not N->is_letter(n2))
        throw myexception()<<"get_doublet requires two exact nucleotide states.";
    int state = doublet_for_components[n1][n2];
    if (state == -1)
        throw myexception()<<"The nucleotide pair is not in this doublet alphabet.";
    return state;
}

int Doublets::sub_nuc(int letter_index, int pos) const
{
    assert( 0 <= pos and pos <= 2);

    return sub_nuc_table[letter_index][pos];
}

vector<string> getDoublets(const vector<string>& v) 
{
    vector<string> w;
    for(int i=0;i<v.size();i++)
    {
	string s1 = v[i];
	for(int j=0;j<v.size();j++)
	{
	    string s2 = s1 + v[j];
	    w.push_back(s2);
	}
    }
    return w;
}

vector<string> getDoublets(const Nucleotides& a)
{
    vector<string> v;
    for(int i=0;i<a.size();i++)
	v.push_back(a.lookup(i));
    return getDoublets(v);
}


bool matches_doublet(const string& c1,const string& c2,const Nucleotides& N)
{
    assert(c1.size() == 2);
    assert(c1.size() == c2.size());

    for(int n=0;n<2;n++)
    {
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
void Doublets::setup_letter_classes() 
{
    // clear masks and classes to just the letters
    alphabet::setup_letter_classes();

    // get nucleotide letters
    vector<string> v = N->letter_classes();
    v.push_back(N->wildcard);

    // construct letter classes names
    vector<string> w = getDoublets(v);
  
    // construct letter class masks
    bitmask_t empty_mask(size());
    bitmask_t mask(size());

    for(int i=0;i<w.size();i++)
    {
	if (contains(w[i])) continue;
	if (w[i] == wildcard) continue;

	mask = empty_mask;

	bool found = false;
	for(int j=0;j<mask.size();j++) {
	    if (::matches_doublet(letter(j),w[i],*N)) {
		mask[j] = true;
		found = true;
	    }
	}
	if (found)
	    insert_class(w[i],mask);
    }
}

valarray<double> get_nucleotide_counts_from_doublet_counts(const Doublets& D,const valarray<double>& D_counts)
{
    const Nucleotides& N = D.getNucleotides();

    valarray<double> N_counts(0.0, N.size());
    // For each codon type
    for(int i=0;i<D.size();i++) {
	// For each position in the codon
	for(int pos=0;pos<2;pos++)
	    // Count the nucleotides that occur there
	    N_counts[ D.sub_nuc(i,pos) ] += D_counts[i];
    }

    return N_counts;
}

valarray<double> get_doublet_frequencies_from_independent_nucleotide_frequencies(const Doublets& D,const valarray<double>& fN )
{
    valarray<double> fD(D.size());
    for(int i=0;i<fD.size();i++) {
	fD[i] = 1.0;
	for(int pos=0;pos<2;pos++)
	    fD[i] *= fN[ D.sub_nuc(i,pos) ];
    }

    fD /= fD.sum();
    return fD;
}

bool Doublets::is_watson_crick(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_watson_crick(d1,d2);
}

bool Doublets::is_mismatch(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_mismatch(d1,d2);
}

bool Doublets::is_wobble_pair(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_wobble_pair(d1,d2);
}

int Doublets::n_changes(int l1, int l2) const
{
    int n = 0;
    for(int pos=0;pos<2;pos++)
	if (sub_nuc(l1,pos) != sub_nuc(l2,pos))
	    n++;
    return n;
}

valarray<double> Doublets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_doublet_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_doublet_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

// Render the positional projections, then compare the states represented by
// their product with the original set to detect lossy Cartesian widening.
std::pair<string, bool> Doublets::lookup(const bitmask_t& mask) const
{
    auto [spelling, exact] = alphabet::lookup(mask);
    if (exact)
        return {spelling, true};

    vector<bitmask_t> projections(2, bitmask_t(N->n_letters()));
    for (auto state = mask.find_first(); state != bitmask_t::npos; state = mask.find_next(state))
        for (int pos = 0; pos < 2; pos++)
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
        if (projections[0][sub_nuc(state, 0)] and projections[1][sub_nuc(state, 1)])
            hull.set(state);
    return {spelling, hull == mask};
}

// Reproduce the product-alphabet diagnostics only after normal encoding fails.
void Doublets::validate_sequence(const string& letters) const
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
    for (int i = 0; i < singlets.size() / letter_size and ok; i++)
    {
        int l1 = singlets[letter_size * i];
        int l2 = singlets[letter_size * i + 1];
        if ((is_feature(l1) and is_feature(l2)) or (l1 == gap and l2 == gap))
            continue;
        e<<" Sequence not aligned as "<<letters_name()<<"!  Column "<<i+1
         <<" has mixed gap/non-gap letter '"<<letters.substr(i * letter_size, letter_size)<<"'";
        ok = false;
    }
    if (n_letters % letter_size != 0)
    {
        if (not ok) e<<"\n";
        e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "
         <<letters_name()<<": not a multiple of 2 "<<N->letters_name()<<"!";
        ok = false;
    }
    if (singlets.size() % letter_size != 0)
    {
        if (not ok) e<<"\n";
        e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "
         <<letters_name()<<": not a multiple of 2 columns!";
        ok = false;
    }
    if (not ok)
        throw e;
}

Doublets::Doublets(const string& s,const Nucleotides& a)
    :alphabet(s,getDoublets(a)),N(a)
{
    // compute our 'wildcard' letter
    wildcard = N->wildcard + N->wildcard;

    // compute our 'gap' letters
    gap_letter = N->gap_letter + N->gap_letter;

    // compute our 'unknown' letters
    unknown_letters.clear();
    for(auto& unknown_letter: N->unknown_letters)
        unknown_letters.push_back( unknown_letter + unknown_letter );

    setup_sub_nuc_table();

    setup_letter_classes();
}

Doublets::Doublets(const Nucleotides& a)
    :Doublets(string("Doublets(")+a.name+")",a)
{ }
