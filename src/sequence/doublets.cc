#include "doublets.H"

using std::vector;
using std::string;
using std::valarray;

void Doublets::setup_sub_nuc_table()
{
    auto n_singlet_letters = N->size();
    doublet_table = vector<vector<int> >(n_singlet_letters,vector<int>(n_singlet_letters,-1));

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

	doublet_table[n0][n1] = i;
    }
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

int Doublets::get_doublet(int n1, int n2) const
{
    if (N->is_feature(n1) and N->is_feature(n2)) 
    {
	if (N->is_letter(n1) and N->is_letter(n2))
	{
	    int index = doublet_table[n1][n2];
	    assert(index != -1);
	    return index;
	}
	else 
	    return alphabet::not_gap;
    }
    else if (n1 == alphabet::gap or n2 == alphabet::gap)
	return alphabet::gap;
    else
	return alphabet::unknown;
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

vector<int> Doublets::operator()(const string& letters) const
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
    vector<int> doublets(singlets.size()/letter_size);

    for(int i=0;i<doublets.size() and ok;i++)
    {
	int l1 = singlets[letter_size * i + 0];
	int l2 = singlets[letter_size * i + 1];

	if (is_feature(l1) and is_feature(l2))
	    doublets[i] = get_doublet(l1, l2);
	else if (l1 == alphabet::gap and l2 == alphabet::gap)
	    doublets[i] = alphabet::gap;
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
	e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "<<letters_name() <<": not a multiple of 2 "<<N->letters_name()<<"!";
	ok = false;
    }

    // 5. Check for extra columns we haven't been using
    if (singlets.size() % letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "<<letters_name() <<": not a multiple of 2 columns!";
	ok = false;
    }

    if (not ok)
	throw e;

    return doublets;
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
