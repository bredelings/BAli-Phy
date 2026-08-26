#include "RNAEdits.hh"
#include "util/set.hh"
#include "util/string/sanitize.hh"

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
    rna_edit_for_components = vector<vector<std::optional<int>>>(
        N->n_letters(), vector<std::optional<int>>(N->n_letters()));
    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++)
    {
	auto& RNAEdit = letter(i);

	assert(RNAEdit.length() == 2);
	sub_nuc_table[i].resize(2);

	auto n0 = N->find_letter(RNAEdit.substr(0,1));
	auto n1 = N->find_letter(RNAEdit.substr(1,1));
        assert(n0 and n1);
	sub_nuc_table[i][0] = *n0;
	sub_nuc_table[i][1] = *n1;
	rna_edit_for_components[*n0][*n1] = i;
    }
}

// Map two exact nucleotide states directly to their exact RNA-edit state, if present.
std::optional<int> RNAEdits::get_doublet(int n1, int n2) const
{
    assert(N->is_letter(n1));
    assert(N->is_letter(n2));
    return rna_edit_for_components[n1][n2];
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

// Diagnose product structure only after encoding fails, so successful input incurs no second scan.
void RNAEdits::diagnose_sequence_encoding_failure(const string& letters) const
{
    const int letter_size = width();
    vector<int> components(letters.size());
    for (int i = 0; i < components.size(); i++)
    {
        string symbol = letters.substr(i, 1);
        if (symbol == N->gap_letter)
            components[i] = gap;
        else if (includes(N->unknown_letters, symbol))
            components[i] = unknown;
        else if (N->mask_for_symbol(symbol))
            components[i] = not_gap;
        else
            throw myexception()<<"Nucleotide symbol '"<<sanitize_string(symbol)<<"' at alignment column "
                               <<i + 1<<" is not recognized by alphabet '"<<N->name<<"'.";
    }

    int first_mixed = -1;
    for (int i = 0; i < components.size() / letter_size; i++)
    {
        if (includes(unknown_letters, letters.substr(letter_size * i, letter_size)))
            continue;
        int l1 = components[letter_size * i];
        int l2 = components[letter_size * i + 1];
        if ((l1 == not_gap and l2 == not_gap) or (l1 == gap and l2 == gap))
            continue;
        first_mixed = i;
        break;
    }

    myexception e;
    bool diagnosed = false;
    if (first_mixed != -1)
    {
        diagnosed = true;
        e<<" Malformed "<<letter_name()<<" at column "<<first_mixed + 1<<" ('"
         <<letters.substr(first_mixed * letter_size, letter_size)
         <<"'): its components mix gap, nongap, or missing symbols.\n"
         <<"   Each "<<letter_name()<<" must be entirely nongap, entirely gaps, or the complete missing symbol.";
    }
    if (components.size() % letter_size != 0)
    {
        if (diagnosed) e<<"\n";
        diagnosed = true;
        e<<" Alignment row has "<<letters.size()<<" columns, but "<<letters_name()
         <<" require a multiple of "<<letter_size<<" columns.";
    }
    if (diagnosed)
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
