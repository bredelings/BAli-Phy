#include "site-compression.H"

#include <optional>
#include <map>
#include "alignment/alignment-util2.H"
#include "alignment/alignment-util.H"

using std::optional;
using std::map;
using std::vector;
using std::tuple;

struct column_map
{
    optional<int> value;
    map<int, column_map> key_first;

    optional<int>& insert(const vector<int>& key, int index=0)
    {
        if (index >= key.size()) return value;
        int x = key[index];
        return key_first[x].insert(key, index+1);
    }
};
        
int find_add_column(column_map& M, const vector<int>& column, int next)
{
    auto& result = M.insert(column);
    if (not result)
        result = next;
    return *result;
}

int add_column(column_map& M, const vector<int>& column, vector<vector<int>>& cols, vector<int>& counts)
{
    assert(cols.size() == counts.size());
    int c = find_add_column(M, column, cols.size());
    if (c == cols.size())
    {
        cols.push_back(column);
        counts.push_back(1);
    }
    else
        counts[c]++;
    return c;
}

vector<int> site_pattern(const alignment& A, int n, int c)
{
    assert(n <= A.n_sequences());

    vector<int> pattern(n);
    for(int j=0;j<n;j++)
    {
        int x = A(c,j);
        if (x < 0) x = alphabet::gap;
        pattern[j] = x;
    }
    return pattern;
}

tuple<vector<vector<int>>,vector<int>,vector<int>> compress_site_patterns(const alignment& A, int n)
{
    column_map M;
    vector<vector<int>> columns;
    vector<int> counts;
    vector<int> mapping(A.length());
    for(int c=0;c<A.length();c++)
        mapping[c] = add_column(M, site_pattern(A,n,c), columns, counts);

    assert(counts.size() == columns.size());
    return {columns, counts, mapping};
}

alignment alignment_from_patterns(const alignment& old, const vector<vector<int>>& patterns, int n_leaves)
{
    assert(n_leaves == patterns[0].size());

    vector<sequence> seqs;
    for(int i=0;i<n_leaves;i++)
        seqs.push_back(sequence(old.seqs()[i].name));

    alignment A(old.get_alphabet(), seqs, patterns.size());

    for(int i=0;i<n_leaves;i++)
        for(int c=0;c<A.length();c++)
            A.set_value(c,i,patterns[c][i]);

    return A;
}

alignment alignment_from_patterns(const alignment& old, const vector<vector<int>>& patterns, const Tree& t)
{
    assert(old.n_sequences() <= t.n_nodes());
    return alignment_from_patterns(old, patterns, t.n_leaves());
}

// This version only returns an alignment with only n sequences (i.e. n is the number of leaf sequence).
compressed_alignment compress_alignment(const alignment& A, int n)
{
    if (A.length() == 0)
        return {A,{},{}};

    auto [patterns, counts, mapping] = compress_site_patterns(A, n);
    return {alignment_from_patterns(A, patterns, n), counts, mapping};
}


// This version returns an alignment with t.n_nodes() sequences
compressed_alignment compress_alignment(const alignment& A, const Tree& t)
{
    auto [patterns, counts, mapping] = compress_site_patterns(A, t.n_leaves());
    return {alignment_from_patterns(A, patterns, t), counts, mapping};
}

alignment uncompress_alignment(const compressed_alignment& A)
{
    return uncompress_alignment(A.compressed, A.mapping);
}

alignment uncompress_alignment(const alignment& compressed, const vector<int>& mapping)
{
    alignment A(compressed.get_alphabet(), compressed.seqs(), mapping.size());

    for(int i=0;i<mapping.size();i++)
        for(int row = 0; row < A.n_sequences(); row++)
            A.set_value(i, row, compressed(mapping[i], row) );

    return A;
}
