#include "RNAEdits.hh"

using std::vector;
using std::string;
using std::valarray;

void RNAEdits::setup_table()
{
    // Edits that are not U->C or C->U
    for(int i=size()-1; i>=0; i--) 
    {
	int n1 = sub_nuc(i,0);
	int n2 = sub_nuc(i,1);

	// non-edits are OK.
	if (n1 == n2) continue;

	// U->C edits are OK.
	if (n1 == N->T() and n2 == N->C()) continue;

	// C->U edits are OK.
	if (n1 == N->C() and n2 == N->T()) continue;

	remove(i);
    }
}

void RNAEdits::setup_sub_nuc_table()
{
    rna_edit_for_components = vector<vector<int>>(N->n_letters(), vector<int>(N->n_letters(), -1));
    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++)
    {
	auto& RNAEdit = letter(i);

	assert(RNAEdit.length() == 2);
	sub_nuc_table[i].resize(2);

	int n0 = sub_nuc_table[i][0] = N->find_letter(RNAEdit.substr(0,1));
	int n1 = sub_nuc_table[i][1] = N->find_letter(RNAEdit.substr(1,1));
	rna_edit_for_components[n0][n1] = i;
    }
}

// Map two exact nucleotide states directly to their exact RNA-edit state.
int RNAEdits::get_doublet(int n1, int n2) const
{
    if (not N->is_letter(n1) or not N->is_letter(n2))
        throw myexception()<<"get_doublet requires two exact nucleotide states.";
    int state = rna_edit_for_components[n1][n2];
    if (state == -1)
        throw myexception()<<"The nucleotide pair is not in this RNA-edit alphabet.";
    return state;
}

int RNAEdits::sub_nuc(int letter_index, int pos) const
{
    assert( 0 <= pos and pos <= 2);

    return sub_nuc_table[letter_index][pos];
}

vector<string> getRNAEdits(const vector<string>& v) 
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

vector<string> getRNAEdits(const Nucleotides& a)
{
    vector<string> v;
    for(int i=0;i<a.size();i++)
	v.push_back(a.lookup(i));
    return getRNAEdits(v);
}


valarray<double> get_nucleotide_counts_from_RNAEdit_counts(const RNAEdits& D,const valarray<double>& D_counts)
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

valarray<double> get_RNAEdit_frequencies_from_independent_nucleotide_frequencies(const RNAEdits& D,const valarray<double>& fN )
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

bool RNAEdits::is_watson_crick(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_watson_crick(d1,d2);
}

bool RNAEdits::is_mismatch(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_mismatch(d1,d2);
}

bool RNAEdits::is_wobble_pair(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_wobble_pair(d1,d2);
}

int RNAEdits::n_changes(int l1, int l2) const
{
    int n = 0;
    for(int pos=0;pos<2;pos++)
	if (sub_nuc(l1,pos) != sub_nuc(l2,pos))
	    n++;
    return n;
}

valarray<double> RNAEdits::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_RNAEdit_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_RNAEdit_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

// Form a state set by filtering valid RNA-edit states through both positional masks.
std::optional<alphabet::bitmask_t> RNAEdits::mask_for_symbol(const string& symbol) const
{
    if (auto mask = alphabet::mask_for_symbol(symbol))
        return mask;
    if (symbol.size() != width())
        return {};

    auto first = N->mask_for_symbol(symbol.substr(0, 1));
    auto second = N->mask_for_symbol(symbol.substr(1, 1));
    if (not first or not second)
        return {};

    bitmask_t mask(n_letters());
    for (int state = 0; state < n_letters(); state++)
        if ((*first)[sub_nuc(state, 0)] and (*second)[sub_nuc(state, 1)])
            mask.set(state);
    if (mask.none())
        return {};
    return mask;
}

// Render the positional projections, then compare the states represented by
// their product with the original set to detect lossy Cartesian widening.
std::pair<string, bool> RNAEdits::lookup(const bitmask_t& mask) const
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
void RNAEdits::validate_sequence(const string& letters) const
{
    const int letter_size = width();
    vector<int> singlets(letters.size());
    for (int i = 0; i < singlets.size(); i++)
    {
        string symbol = letters.substr(i, 1);
        if (symbol == N->gap_letter)
            singlets[i] = gap;
        else
        {
            bool is_unknown = false;
            for (const auto& unknown_letter: N->unknown_letters)
                is_unknown = is_unknown or symbol == unknown_letter;
            if (is_unknown)
                singlets[i] = unknown;
            else if (N->mask_for_symbol(symbol))
                singlets[i] = not_gap;
            else
                throw bad_letter(symbol, N->name);
        }
    }

    int n_letters = 0;
    for (auto singlet: singlets)
        if (singlet == not_gap)
            n_letters++;

    myexception e;
    bool ok = true;
    for (int i = 0; i < singlets.size() / letter_size and ok; i++)
    {
        int l1 = singlets[letter_size * i];
        int l2 = singlets[letter_size * i + 1];
        if ((l1 == not_gap and l2 == not_gap) or (l1 == gap and l2 == gap))
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

RNAEdits::RNAEdits(const string& s,const Nucleotides& a)
    :alphabet(s,getRNAEdits(a)),N(a)
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

    setup_table();

    setup_sub_nuc_table();
}

RNAEdits::RNAEdits(const Nucleotides& a)
    :RNAEdits(string("RNAEdits(")+a.name+")",a)
{ }
