#include "site-compression.H"

#include <optional>
#include <map>
#include "alignment/alignment-util2.H"

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

alignment alignment_from_patterns(const alignment& old, const vector<vector<int>>& patterns, const TreeInterface& t)
{
    assert(old.n_sequences() <= t.n_nodes());
    assert(t.n_leaves() == patterns[0].size());
    assert(old.seqs().size() == t.n_nodes());

    alignment A(old.get_alphabet(), old.seqs(), patterns.size());

    for(int i=0;i<t.n_nodes();i++)
        if (i < t.n_leaves())
            for(int c=0;c<A.length();c++)
                A.set_value(c,i,patterns[c][i]);
        else
            for(int c=0;c<A.length();c++)
                A.set_value(c,i,alphabet::gap);

    minimally_connect_leaf_characters(A,t);
    return A;
}

tuple<alignment, vector<int>, vector<int>>
compress_alignment(const alignment& A, const TreeInterface& t)
{
    auto [patterns, counts, mapping] = compress_site_patterns(A, t.n_leaves());
    return {alignment_from_patterns(A, patterns, t), counts, mapping};
}


