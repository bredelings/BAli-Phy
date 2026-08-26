#include "triplets.hh"
#include "codons.hh"
#include "util/set.hh"
#include "util/string/sanitize.hh"

using std::vector;
using std::string;
using std::valarray;

void Triplets::setup_sub_nuc_table()
{
    triplet_for_components = vector<vector<vector<std::optional<int>>>>(
        N->n_letters(), vector<vector<std::optional<int>>>(
                            N->n_letters(), vector<std::optional<int>>(N->n_letters())));
    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++) {
	const string& codon = letter(i);

	assert(codon.length() == 3);
	sub_nuc_table[i].resize(3);

	auto n0 = N->find_letter(codon.substr(0,1));
	auto n1 = N->find_letter(codon.substr(1,1));
	auto n2 = N->find_letter(codon.substr(2,1));
        assert(n0 and n1 and n2);
	sub_nuc_table[i][0] = *n0;
	sub_nuc_table[i][1] = *n1;
	sub_nuc_table[i][2] = *n2;
	triplet_for_components[*n0][*n1][*n2] = i;
    }
}

// Map three exact nucleotide states directly to their exact triplet state, if present.
std::optional<int> Triplets::get_triplet(int n1, int n2, int n3) const
{
    assert(N->is_letter(n1));
    assert(N->is_letter(n2));
    assert(N->is_letter(n3));
    return triplet_for_components[n1][n2][n3];
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

// Form a state set by filtering the exact triplets through all positional masks.
std::optional<alphabet::bitmask_t> Triplets::mask_for_symbol(const string& symbol) const
{
    if (auto mask = alphabet::mask_for_symbol(symbol))
        return mask;
    if (symbol.size() != width())
        return {};

    auto first = N->mask_for_symbol(symbol.substr(0, 1));
    auto second = N->mask_for_symbol(symbol.substr(1, 1));
    auto third = N->mask_for_symbol(symbol.substr(2, 1));
    if (not first or not second or not third)
        return {};

    bitmask_t mask(n_letters());
    for (int state = 0; state < n_letters(); state++)
        if ((*first)[sub_nuc(state, 0)] and (*second)[sub_nuc(state, 1)] and
            (*third)[sub_nuc(state, 2)])
            mask.set(state);
    if (mask.none())
        return {};
    return mask;
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

// Diagnose product structure and excluded exact states only after encoding fails.
void Triplets::diagnose_sequence_encoding_failure(const string& letters) const
{
    const int letter_size = width();
    vector<int> components(letters.size());
    vector<int> exact_states(letters.size(), -1);
    for (int i = 0; i < components.size(); i++)
    {
        string symbol = letters.substr(i, 1);
        if (symbol == N->gap_letter)
            components[i] = gap;
        else if (includes(N->unknown_letters, symbol))
            components[i] = unknown;
        else if (auto state = N->find_letter(symbol))
        {
            components[i] = not_gap;
            exact_states[i] = *state;
        }
        else if (N->mask_for_symbol(symbol))
            components[i] = not_gap;
        else
            throw myexception()<<"Nucleotide symbol '"<<sanitize_string(symbol)<<"' at alignment column "
                               <<i + 1<<" is not recognized by alphabet '"<<N->name<<"'.";
    }

    int first_mixed = -1;
    vector<int> stop_codons;
    for (int i = 0; i < components.size() / letter_size; i++)
    {
        if (includes(unknown_letters, letters.substr(letter_size * i, letter_size)))
            continue;
        int l1 = components[letter_size * i];
        int l2 = components[letter_size * i + 1];
        int l3 = components[letter_size * i + 2];
        if (l1 == not_gap and l2 == not_gap and l3 == not_gap)
        {
            int n1 = exact_states[letter_size * i];
            int n2 = exact_states[letter_size * i + 1];
            int n3 = exact_states[letter_size * i + 2];
            if (n1 >= 0 and n2 >= 0 and n3 >= 0 and not triplet_for_components[n1][n2][n3])
                stop_codons.push_back(i);
        }
        else if (l1 == gap and l2 == gap and l3 == gap)
            continue;
        else
        {
            if (first_mixed == -1)
                first_mixed = i;
        }
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
    if (not stop_codons.empty())
    {
        if (diagnosed) e<<"\n";
        diagnosed = true;
        auto codons = dynamic_cast<const Codons*>(this);
        assert(codons);
        e<<" Sequence contains "<<stop_codons.size()<<" stop codons under genetic code '"
         <<codons->getGenetic_Code().name()<<"'.\n";
        int column = stop_codons.front();
        e<<"   First is '"<<letters.substr(column * letter_size, letter_size)<<"' at "
         <<letter_name()<<" column "<<column + 1<<" (nucleotide column "<<3 * column + 1<<").";
    }
    if (diagnosed)
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
}

Triplets::Triplets(const Nucleotides& a)
    :Triplets(string("Triplets(")+a.name+")",a)
{ }
