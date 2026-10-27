/*
  Copyright (C) 2005-2010,2012,2017 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

#include <iostream>
#include <map>
#include <string>
#include <regex>
#include "alignment/alignment.hh"
#include "alignment/load.hh"
#include "alignment/alignment-util.hh"
#include "util/string/split.hh"
#include "util/string/join.hh"
#include <CLI/CLI.hpp>
#include <limits>
#include <optional>
#include <boost/dynamic_bitset.hpp>
#include "util/assert.hh"
#include "util/io.hh"
#include "util/cmdline.hh"
#include "util/mapping.hh"
#include "util/log-level.hh"
#include "util/string/convert.hh"
#include "util/string/pred.hh"
#include "util/string/strip.hh"
#include "util/range.hh"
#include "util/matrix.hh"
#include "util/io/matrix.hh"

#include <algorithm>
#include <ranges>

using std::vector;
using std::valarray;
using std::set;
using std::map;
using std::pair;
using std::string;
using std::endl;
using std::ostream;
using std::optional;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;

struct sequence_mask
{
    // Name
    string region_name;
    // 0-based regions to mask out.
    vector<pair<int,int>> intervals;
};

namespace {
// Settings shared by the mutually exclusive analysis commands and their preprocessing pipeline.
struct preprocessing_options
{
    string alphabet_name;
    bool erase_empty = false;
    optional<string> reference;
    bool autoclean = false;
    vector<string> mask_files;
    optional<int> mask_gap_distance;
    double gap_fraction = 0.01;
    bool strip_gaps = false;
    vector<string> mask_allele_specs;
    string consensus_seqs = "-";
};
}

// Register shared settings on each analysis command, keeping its help self-contained.
static void add_preprocessing_options(CLI::App& command, preprocessing_options& options)
{
    command.add_option("--alphabet", options.alphabet_name, "Alphabet (default: inferred from alignment)");
    command.add_flag("--erase-empty-columns,-e", options.erase_empty, "Remove empty alignment columns");
    command.add_option("--clean-to-ref", options.reference, "Remove columns absent from this reference sequence");
    command.add_flag("--autoclean,-A", options.autoclean, "Mask blocks with too many SNPs");
    command.add_option("--mask-file,-M", options.mask_files, "Apply reference-coordinate mask files; may be repeated");
    command.add_option("--mask-gaps,-G", options.mask_gap_distance, "Mask columns within this distance of a gap");
    command.add_option("--gap-fraction", options.gap_fraction, "Gap fraction used by --mask-gaps")
        ->capture_default_str();
    command.add_flag("--strip-gaps,-S", options.strip_gaps,
                     "With --mask-gaps, remove gap columns and mask nearby columns");
    command.add_option("--mask-alleles,-m", options.mask_allele_specs,
                       "Mask alleles with N differences in L columns; may be repeated")->type_name("N:L");
    command.add_option("--consensus-seqs", options.consensus_seqs,
                       "Sequence range for consensus (default: all sequences)")->type_name("RANGE");
}


// Number of alphbat letters with count > level
int n_letters(const valarray<int>& count,int level) {
    int n = 0;
    for(int l=0;l<count.size();l++)
        if (count[l] > level) n++;
    return n;

}

bool is_informative(const valarray<int>& count,int level) 
{
    int n = n_letters(count,level);
    return n>1;
}


vector<int> diffuse(const vector<int>& v, int d)
{
    auto w = v;

    for(int i=0;i<w.size();i++)
    {
        if (w[i]) continue;

        for(int j=1;j<=d and not w[i];j++)
        {
            if (i+j >=0 and i+j < v.size() and v[i+j])
                w[i] = true;
            else if (i-j >=0 and i-j < v.size() and v[i-j])
                w[i] = true;
        }
    }
    return w;
}

/*
NUM LOCI
20

SEGREGATING SITES
2       4       8       14

HAPLOTYPES
1100
0011
1010
0101
0001
1000
1010
0101
*/
void write_dical2(std::ostream& o, const alignment& A)
{
    vector<vector<int>> haplotypes(A.n_sequences());
    vector<int> segregating_sites;
    for(int column=0;column<A.length();column++)
    {
        if (is_masked_column(A,column)) continue;

        if (not is_variant_column(A,column)) continue;
        
        // DiCal2's binary encoding cannot distinguish multiple alternate exact alleles.
        vector<bool> observed(A.get_alphabet().size(), false);
        for(int h=0;h<A.n_sequences();h++)
            if (A.get_alphabet().is_letter(A(column,h)))
                observed[A(column,h)] = true;
        if (std::count(observed.begin(), observed.end(), true) > 2) continue;

        segregating_sites.push_back(column+1);

        for(int h=0;h<A.n_sequences();h++)
            haplotypes[h].push_back( A(column,h) == A(column,0)?0:1 );
    }
    // Write number of loci
    o<<"NUM LOCI\n"<<A.length()<<"\n\n";

    // Write snp locations
    o<<"SEGREGATING SITES\n";
    join(o, segregating_sites, '\t');
    o<<"\n\n";
        
    // write haplotypes
    o<<"HAPLOTYPES\n";
    for(auto& haplotype: haplotypes)
    {
        join(o,haplotype,"");
        o<<"\n";
    }
    o.flush();
}


/*
 https://github.com/stschiff/msmc/blob/master/guide.md
1   58432   63  TCCC
1   58448   16  GAAA
1   68306   15  CTTT
1   68316   10  TCCC
1   69552   8   GCCC
1   69569   17  TCCC
1   801848  9730    CCCA
1   809876  1430    AAAG
1   825207  1971    CCCT,CCTC
1   833223  923 TCCC
*/

void write_msmc(std::ostream& o, const alignment& A)
{
    int called_sites = 0;
    int widened = 0;
    for(int i=0;i<A.length();i++)
    {
        // Count unmasked sites since the preceding output SNP, including this site.
        // A mask contributes zero but does not discard earlier callable positions.
        if (is_masked_column(A,i)) continue;
        called_sites++;

        if (not is_variant_column(A,i)) continue;
        
        o<<"1\t"<<i+1<<"\t"<<called_sites<<"\t";
        for(int j=0;j<A.n_sequences();j++)
        {
            auto [spelling, exact] = A.decode(A(i,j));
            o<<spelling;
            widened += not exact;
        }
        o<<"\n";

        called_sites = 0;
    }

    if (widened)
        std::cerr<<"Warning: widened "<<widened
                 <<" non-Cartesian or unspellable ambiguities while writing sequences.\n";
}

void write_psmc(std::ostream& o, const alignment& A, int x1, int x2)
{
    const auto& a = A.get_alphabet();
    constexpr int blocksize = 1000;
    o<<">1\n";
    for(int i=0;i<A.length()/blocksize;i++)
    {
        bool diff = false;
        bool observed = false;
        for(int j=0;not diff and j<blocksize;j++)
        {
            int c = blocksize * i + j;

            int l1 = A(c,x1);
            int l2 = A(c,x2);

            if (not a.is_letter(l1) or not a.is_letter(l2)) continue;

            observed = true;
            diff = (l1 != l2);
        }
        if (diff)
            o<<"K";
        else
            o<<(observed ? "T" : "N");
        if (i%60 == 59)
            o<<"\n";
    }
    o<<std::endl;
}


int n_snps(const alignment& A)
{
    int count = 0;
    for(int i=0;i<A.length();i++)
        if (is_variant_column(A,i))
            count++;
    return count;
}

vector<int> snps_in_blocks(const alignment& A, int blocksize)
{
    if (blocksize <= 0)
        throw myexception()<<"--histogram requires a positive block size.";
    vector<int> counts;
    for(int i=0;i<A.length();)
    {
        int count=0;
        int end = i+blocksize;
        for(;i<A.length() and i<end;i++)
            if (is_variant_column(A,i))
                count++;
        counts.push_back(count);
    }
    return counts;
}

void write_histogram(std::ostream& o, int blocksize, const alignment& A)
{
    for(int count: snps_in_blocks(A,blocksize))
        o<<count<<"\n";
}


// The counts should be Poisson distributed.  It will be relatively rare to get
// counts more than 10 times higher than the mean

dynamic_bitset<> block_mask(dynamic_bitset<> mask, int blocksize)
{
    auto mask2 = mask;
    for(int i=1; i<=blocksize; i++)
    {
        mask2 >>= 1;
        mask |= mask2;
    }
    return mask;
}

alignment clean_to_ref(alignment& A, const string& ref_name)
{
    auto index = find_index(sequence_names(A), ref_name);

    if (not index)
        throw myexception()<<"Can't find reference name '"<<ref_name<<"' in alignment!";

    vector<int> cols;
    for(int c=0;c<A.length();c++)
        if (A.character(c,*index))
            cols.push_back(c);

    return select_columns(A,cols);
}

int autoclean(alignment& A)
{
    constexpr double mean_snps_per_block = 2.0;
    constexpr double factor = 10.0;
    int n_sites = A.length();
    int S = n_snps(A);
    if (S < 100) throw myexception()<<"Refusing to autoclean chromosome with < 100 variant sites.";

    int masked = 0;
    int blocksize = int(n_sites*(mean_snps_per_block/S)+0.5);

    dynamic_bitset<> mask(n_sites);
    int count = 0;
    for(int c2=0; c2<n_sites; c2++)
    {
        int c1 = c2 - blocksize;

        if (            is_variant_column(A,c2)) count++;
        if (c1 >= 0 and is_variant_column(A,c1)) count--;

        assert(count == count_variant_columns(A, std::max(c1+1,0), c2));

        if (count >= mean_snps_per_block*factor)
        {
            mask.set(c2);
            masked++;
        }
    }

    auto mask2 = block_mask(mask, blocksize);
    for(int i=0;i<n_sites;i++)
    {
        if (mask2[i])
        {
            mask_column(A, i);
        }
    }

//    std::cerr<<"Masked "<<masked<<" sites\n";

    return masked;
}




void mask_one_sequence(alignment& A, int i, const vector<int>& columns)
{
    for(int col: columns)
    {
        if (A.character(col,i))
            A.set_value(col, i, alphabet::not_gap);
    }
}

void mask_interval(alignment& A, int c1, int c2)
{
    for(int c = c1; c <= c2; c++)
        mask_column(A,c);
}

optional<int> find_mask_target(const vector<string>& seq_names, string mask_name)
{
    string suffix = "-"+mask_name;
    for(int i=0; i<seq_names.size(); i++)
    {
        if (seq_names[i] == mask_name) return i;
        if (ends_with(seq_names[i], suffix)) return i;
    }
    return {};
}

void apply_mask(const sequence_mask& mask, alignment& A)
{
    auto index = find_mask_target(sequence_names(A), mask.region_name);
    if (not index)
        throw myexception()<<"Can't apply mask for region '"<<mask.region_name<<"': no such sequence!";

    vector<int> map;
    for(int c=0;c<A.length();c++)
        if (A.character(c,*index))
            map.push_back(c);
    map.push_back(map.back());

    for(auto& [beg,end]: mask.intervals)
        mask_interval(A, map[beg], map[end]);
}

void apply_masks(const vector<sequence_mask>& masks, alignment& A)
{
    for(auto& mask: masks)
        apply_mask(mask, A);
}

matrix<double> pi_matrix(const alignment& A)
{
    auto& a = A.get_alphabet();
    const int n = A.n_sequences();
    matrix<int> total(n,n,0);
    matrix<int> diff(n,n,0);

    for(int col=0;col<A.length();col++)
        for(int i=0;i<n;i++)
            for(int j=0;j<i;j++)
            {
                auto l_i = A(col, i);
                auto l_j = A(col, j);
                if (a.is_letter(l_i) and a.is_letter(l_j))
                {
                    total(i,j)++;
                    if (l_i != l_j)
                        diff(i,j)++;
                }
            }
    matrix<double> pi(n,n);
    for(int i=0;i<n;i++)
    {
        pi(i,i) = 0;
        for(int j=0;j<i;j++)
            pi(i,j) = pi(j,i) = double(diff(i,j))/total(i,j);
    }
    return pi;
}

double pi(const alignment& A)
{
    auto& a = A.get_alphabet();
    valarray<int> count(a.size());

    long int pi_count = 0;
    long int pi_total = 0;
    for(int c=0;c<A.length();c++)
    {
        count = 0;
        for(int i=0;i<A.n_sequences();i++)
        {
            int l = A(c,i);
            if (a.is_letter(l))
                count[l]++;
        }

        int total_letters = count.sum();
        pi_total += total_letters*(total_letters-1)/2;

        for(int l1=0;l1<a.size();l1++)
            for(int l2=0;l2<l1;l2++)
                pi_count += count[l1]*count[l2];
    }

    return double(pi_count)/pi_total;
}

// This is for dust-masker-style masks.
vector<sequence_mask> read_masks(const string& filename)
{
    // 1. Read fine lines
    checked_ifstream file(filename,"mask file");

    auto lines = read_lines(file);
    lines = select(lines, [](auto& line){return not strip(line," \t").empty();});

    // 2. Read ranges
    vector<sequence_mask> masks;

    static std::regex rgx ( R"(\s*([0-9]+)\s*-\s*([0-9]+)\s*)" );
    for(auto& line: lines)
    {
        if (line.size() > 0 and line[0] == '>')
        {
            masks.push_back({});
            masks.back().region_name = rstrip(line.substr(1), " \t");
            continue;
        }
        else if (masks.empty())
            throw myexception()<<"Read line '"<<line<<"' before first header!";
        
        try {
            std::smatch m;
            if (std::regex_match(line, m, rgx))
            {
                int beg = convertTo<int>(m[1]);
                int end = convertTo<int>(m[2]);
                masks.back().intervals.push_back({beg,end});
            }
            else
                throw myexception()<<"malformed interval!";
        }
        catch (myexception& e)
        {
            e.prepend("Interval '"+line+"': ");
            throw;
        }
    }

    return masks;
}

vector<int> allele_counts(const alignment& A, int col)
{
    auto& a = A.get_alphabet();
    vector<int> counts(a.size(), 0);
    for(int i=0;i<A.n_sequences();i++)
    {
        auto c = A(col,i);
        if (a.is_letter(c))
            counts[c]++;
    }
    return counts;
}

vector<int> allele_counts(const alignment& A, int col, const vector<int>& seqs)
{
    auto& a = A.get_alphabet();
    vector<int> counts(a.size(), 0);
    for(int i:seqs)
    {
        auto c = A(col,i);
        if (a.is_letter(c))
            counts[c]++;
    }
    return counts;
}

vector<int> allele_counts_with_gap(const alignment& A, int col, const vector<int>& seqs)
{
    auto& a = A.get_alphabet();
    vector<int> counts(a.size()+2, 0);
    const int not_gap = a.size()+1;
    const int gap = a.size();
    for(int i:seqs)
    {
        auto c = A(col,i);
        if (a.is_letter(c))
            counts[c]++;
        else if (c == alphabet::gap)
            counts[gap]++;
        else if (c == alphabet::not_gap)
            counts[not_gap]++;
    }
    return counts;
}

vector<vector<int>> make_bins(const vector<int>& columns, int width)
{
    if (width <= 0)
        throw myexception()<<"--one-every requires a positive width.";
    vector<vector<int>> bins;
    if (columns.empty()) return bins;

    int end = -1;
    for(int column: columns)
    {
        while (column >= end)
        {
            end += width;
            bins.push_back({});
        }
        bins.back().push_back(column);
    }

    return bins;
}

double missing_freq(const alignment& A, int col)
{
    int n_missing = 0;
    for(int i=0;i<A.n_sequences();i++)
    {
        if (A.missing(col,i))
            n_missing++;
    }
    return double(n_missing)/A.n_sequences();
}

int largest_minor_allele_count(const alignment& A, int col)
{
    auto counts = allele_counts(A, col);

    int m1 = std::max(counts[0],counts[1]);
    int m2 = std::min(counts[0],counts[1]);
    for(int i=2;i<counts.size();i++)
    {
        int count = counts[i];
        if (count > m1)
        {
            m2 = m1;
            m1 = count;
            continue;
        }
        else if (count > m2)
            m2 = count;
    }
    return m2;
}

vector<int> get_consensus(const alignment& A, const vector<int>& seqs)
{
    auto& a = A.get_alphabet();
    const int not_gap = a.size()+1;
    const int gap = a.size();

    // The consensus is not going to have ambiguity
    vector<int> consensus(A.length(), alphabet::unknown);

    for(int col=0; col < A.length(); col++)
    {
        auto counts = allele_counts_with_gap(A, col, seqs);

        if (counts[gap]*2 > seqs.size())
            consensus[col] = alphabet::gap;
        else
        {
            int total_non_gap = counts[not_gap];
            for(int letter = 0; letter < a.size(); letter++)
                total_non_gap += counts[letter];

            for(int letter = 0; letter < a.size(); letter++)
                if (counts[letter]*2 > total_non_gap)
                    consensus[col] = letter;

            if (consensus[col] == alphabet::unknown)
                consensus[col] = alphabet::not_gap;
        }
    }

    return consensus;
}

vector<int> get_consensus_strict(const alignment& A, const vector<int>& seqs)
{
    auto& a = A.get_alphabet();
    const int not_gap = a.size()+1;
    const int gap = a.size();

    // The consensus is not going to have ambiguity
    vector<int> consensus(A.length(), alphabet::gap);

    for(int col=0; col < A.length(); col++)
    {
        auto counts = allele_counts_with_gap(A, col, seqs);

        int total_non_gap = counts[not_gap];
        for(int letter = 0; letter < a.size(); letter++)
            total_non_gap += counts[letter];

        if (counts[gap] < total_non_gap)
        {
            // drop - and +
            counts.resize(a.size());

            // Missing bases establish presence, but cannot select an exact consensus letter.
            if (std::any_of(counts.begin(), counts.end(), [](int count) {return count > 0;}))
                consensus[col] = argmax(counts);
            else
                consensus[col] = alphabet::not_gap;
        }
    }

    return consensus;
}

vector<pair<int,int>> get_non_zero_allele_counts2(const alignment& A, int col, const vector<int>& seqs)
{
    auto counts = allele_counts(A, col, seqs);

    vector<pair<int,int>> allele_count;
    for(int i = 0; i< counts.size(); i++)
        if (counts[i] > 0)
            allele_count.push_back({i,counts[i]});

    return allele_count;
}

vector<pair<int,int>> get_allele_counts2(const alignment& A, int col)
{
    auto counts = allele_counts(A, col);

    vector<pair<int,int>> allele_count;
    for(int i=0; i < counts.size(); i++)
        allele_count.push_back({i,counts[i]});

    // Sort so that greatest count is at the beginning.
    std::sort(allele_count.begin(), allele_count.end(), [](auto x, auto y) {return x.second > y.second;});

    return allele_count;
}

pair<int,int> get_ref_alt_index(const alignment& A, int col)
{
    auto alleles = get_allele_counts2(A, col);

    if (alleles.size() < 2)
        throw myexception()<<"Column "<<col<<" isn't variable!";

    return {alleles[0].first, alleles[1].first};
}

pair<string,string> get_ref_alt(const alignment& A, int col)
{
    auto [ref,alt] = get_ref_alt_index(A, col);

    auto& a = A.get_alphabet();
    return {a.lookup(ref), a.lookup(alt)};
}

ostream& write_snp_bed(ostream& o, const string& chromosome, const vector<int>& columns, const alignment& A)
{
    o<<"#CHROM\tPOS\tID\tREF\tALT\n";
    for(int column: columns)
    {
        auto [ref,alt] = get_ref_alt(A, column);
        o<<chromosome<<'\t'<<column<<'\t'<<column+1<<'\t'<<ref<<'\t'<<alt<<'\n';
    }
    return o;
}

set<string> get_populations(const alignment& A)
{
    set<string> pops;
    auto names = sequence_names(A);
    for(auto& name: names)
    {
        auto l = name.find(':');
        if (l != string::npos)
            pops.insert(name.substr(0,l));
    }
    return pops;
}

vector<int> get_indices_for_population(const string& name, const alignment& A)
{
    auto names = sequence_names(A);
    vector<int> indices;
    string prefix = name + ":";
    for(int i=0; i<names.size(); i++)
        if (starts_with(names[i], prefix))
            indices.push_back(i);
    return indices;
}

optional<int> get_frequency(const alignment& A, int allele, int column, const vector<int>& indices)
{
    int count = 0;
    int total = 0;
    for(int i: indices)
    {
        if (not A.get_alphabet().is_letter(A(column,i))) continue;

        total++;
        if (A(column,i) == allele)
            count++;
    }
    if (total == 0)
        return {};
    else
    {
        double p = 100*count/total;
        return (int)p;
    }
}

ostream& write_snp_bed_with_frequencies(ostream& o, const string& chromosome, const vector<int>& columns, const alignment& A)
{
    vector<string> fields = {"#CHROM","POS","ID","REF","ALT"};
    auto populations = get_populations(A);
    fields.push_back("FREQ");
    for(auto& pop: populations)
        fields.push_back("FREQ_"+pop);
    join(o, fields, '\t')<<"\n";
    vector<vector<int>> indices;
    indices.push_back( iota<int>(A.n_sequences()) );
    for(auto& pop: populations)
        indices.push_back( get_indices_for_population(pop, A));
    
    for(int column: columns)
    {
        auto [ref,alt] = get_ref_alt_index(A, column);
        auto& a = A.get_alphabet();
        o<<chromosome<<'\t'<<column<<'\t'<<column+1<<'\t'<<a.lookup(ref)<<'\t'<<a.lookup(alt);
        for(auto& v: indices)
        {
            o<<'\t';
            auto freq = get_frequency(A, alt, column, v);
            if (freq)
                o<<*freq;
            else
                o<<"NA";
        }
        o<<'\n';
    }
    return o;
}

struct translation_table
{
    string source_chrom;
    string target_chrom;
    vector<pair<int,int>> lookup;

    translation_table(const sequence& s1, const sequence& s2)
        :source_chrom(s1.name),
         target_chrom(s2.name)
    {
        if (s1.size() != s2.size())
            throw myexception()<<"Cannot translation '"<<s1.name<<"' -> '"<<s2.name<<"': lengths "<<s1.size()<<" and "<<s2.size()<<" differ.";

        int loc2 = -1;
        for(int i=0; i<s1.size(); i++)
        {
            bool is_char1 = s1[i] != '-';
            bool is_char2 = s2[i] != '-';

            // -1 + (How many characters of sequence 2 have we seen);
            if (is_char2) loc2++;

            // This map is indexed by sequence 1, so we only add entries if sequence 1 is present.
            if (not is_char1) continue;

            if (is_char2)
                lookup.push_back({loc2,loc2});
            else
                lookup.push_back({loc2,loc2+1});
        }


    }
};

map<string,translation_table> get_translation_tables(const string& filename)
{
    istream_or_ifstream translation_file(std::cin, "-", filename);
    auto sequences = sequence_format::read_fasta_entire_file(translation_file);

    map<string,translation_table> tables;

    for(int i=0; i<sequences.size(); i+=2)
    {
        if (i+1 >= sequences.size()) throw myexception()<<"Odd number of sequences!";

        auto& s1 = sequences[i];
        auto& s2 = sequences[i+1];

        if (tables.count(s1.name)) throw myexception()<<"Translation alignment for '"<<s1.name<<"' occurs twice!";

        tables.insert({s1.name, translation_table(s1,s2)});
    }
    return tables;
}

vector<int> find_sequence_subset_with_prefix(const alignment& a, const string& prefix)
{
    vector<int> sequence_indices;
    for(int i=0;i<a.n_sequences();i++)
        if (starts_with(a.seq(i).name, prefix))
            sequence_indices.push_back(i);
    return sequence_indices;
}

int get_derived_count(const alignment& A, int col, const vector<int>& pop, int ancestral_allele, int derived_allele)
{
    auto pop_counts = get_non_zero_allele_counts2(A, col, pop);

    assert(pop_counts.size() >= 1 and pop_counts.size() <= 2);

    for(auto& [allele,count]: pop_counts)
        if (allele == derived_allele)
            return count;
        else if (allele != ancestral_allele)
            std::abort();

    return 0;
}

matrix<int> compute_2d_sfs(const alignment& A,
                           const vector<int>& pop1,
                           const vector<int>& pop2,
                           const vector<int>& anc)
{
    matrix<int> counts(pop1.size()+1, pop2.size()+1,0);

    auto pop12 = pop1;
    pop12.insert(pop12.end(), pop2.begin(), pop2.end());

    auto all = pop12;
    all.insert(all.end(), anc.begin(), anc.end());

    for(int i=0;i<A.length();i++)
    {
        // Require 2 alleles in the populations
        auto pop_counts = get_non_zero_allele_counts2(A, i, pop12);
        if (pop_counts.size() != 2) continue;

        // Require 2 alleles in the populations and the ancestor.
        auto all_counts = get_non_zero_allele_counts2(A, i, all);
        if (all_counts.size() != 2) continue;

        // Require 1 allele in the ancestral populations
        auto anc_counts = get_non_zero_allele_counts2(A, i, anc);
        if (anc_counts.size() != 1) continue;

        // The matrix axes use fixed population sizes, so every sampled member must be called.
        // Otherwise an unobserved allele would be treated implicitly as ancestral.
        bool missing_data = false;
        for(int j: pop12)
            if (not A.get_alphabet().is_letter(A(i,j)))
                missing_data = true;

        if (missing_data) continue;

        // Then its easy to find "the" ancestral allele
        auto ancestral_allele = anc_counts[0].first;

        // Then we have to find the derived allele
        assert(all_counts[0].first == ancestral_allele or all_counts[1].first == ancestral_allele);
        int derived_allele = all_counts[0].first;
        if (derived_allele == ancestral_allele)
            derived_allele = all_counts[1].first;
        assert(ancestral_allele != derived_allele);
        assert(all_counts[0].first == derived_allele or all_counts[1].first == derived_allele);

        int derived_count1 = get_derived_count(A, i, pop1, ancestral_allele, derived_allele);
        int derived_count2 = get_derived_count(A, i, pop2, ancestral_allele, derived_allele);

        counts(derived_count1, derived_count2)++;
    }
    
    return counts;
}

vector<pair<int,int>> get_snp_positions(const alignment& A, int i, int j)
{
    auto& a = A.get_alphabet();

    vector<pair<int,int>> positions;

    int i_pos=0;
    int consensus_pos=0;
    for(int c=0;c<A.length();c++)
    {
        int li = A(c,i);
        int lj = A(c,j);
        if (a.is_letter(li) and a.is_letter(lj) and li != lj)
            positions.push_back({consensus_pos,i_pos});

        if (not A.gap(c,i))
            i_pos++;
        if (not A.gap(c,0))
            consensus_pos++;
    }

    return positions;
}

// Record differing exact letters by alignment column so consensus lookup remains unambiguous.
vector<pair<int,int>> get_snps_versus_consensus(const alignment& A, const vector<int>& consensus, int i)
{
    auto& a = A.get_alphabet();
    vector<pair<int,int>> positions;
    for(int c=0;c<A.length();c++)
    {
        int li = A(c,i);
        int lc = consensus[c];
        if (a.is_letter(li) and a.is_letter(lc) and li != lc)
            positions.push_back({c, li});
    }
    return positions;
}


vector<int> get_diff_columns(const alignment& A, const vector<int>& consensus, int i)
{
    vector<int> columns;

    for(int c=0;c<A.length();c++)
    {
        if (not A.consistent(A(c,i), consensus[c]))
            columns.push_back(c);
    }

    return columns;
}

/*
 * How about cases where we have >=n snps in L bases, but n UNIQUE snps in L bases?  Does thus make sense if looking for a count of (say) 2 for each base?
 * Possibly, we could just pass min_count and max_count in to get_n_snps_versus consensus?
 */
vector<int> get_allele_columns(const vector<int>& consensus, const alignment& A, int seq_index, int n_snps, int L_max)
{
    assert(consensus.size() == A.length());
    assert(n_snps > 1);
    assert(L_max > 1);

    vector<int> columns;

    // The position here is in columns.
    // This is probably OK, since any insertions of gaps would count as a difference, unless that
    //   is the consensus.
    auto snps = get_diff_columns(A, consensus, seq_index);

    int column = -1;
    for(int snp_index=0; snp_index<(int)snps.size()-n_snps+1; snp_index++)
    {
        // number of columns from the (snp_index)-th SNP to the (snp_index+n_snps-1)-th SNP.
        int c1 = snps[snp_index];
        int c2 = snps[snp_index+n_snps-1];
        int L = c2 - c1 + 1;

        if (L <= L_max)
        {
            for(int c = std::max(column+1,c1); c <= c2; c++)
            {
                columns.push_back(c);
                column = c;
            }
        }
    }

    return columns;
}

vector<int> merge_columns(const vector<int>& c1, const vector<int>& c2)
{
    vector<int> out;
    int i1 = 0;
    int i2 = 0;
    while (i1 < c1.size() or i2 < c2.size())
    {
        if (i1 == c1.size())
            out.push_back(c2[i2++]);
        else if (i2 == c2.size())
            out.push_back(c1[i1++]);
        else if (c1[i1] < c2[i2])
            out.push_back(c1[i1++]);
        else if (c1[i1] > c2[i2])
            out.push_back(c2[i2++]);
        else
        {
            int C = c1[i1];
            out.push_back(C);
            i1++;
            i2++;
        }
    }

    assert(out.size() >= c1.size());
    assert(out.size() >= c2.size());
    assert(out.size() <= c1.size() + c2.size());

    return out;
}

vector<int> get_allele_columns(const vector<int>& consensus, const alignment& A, int seq_index, const vector<pair<int,int>>& snp_lengths)
{
    vector<int> columns;
    for(auto& [n,L]: snp_lengths)
    {
        auto columns2 = get_allele_columns(consensus, A, seq_index, n, L);
        columns = merge_columns(columns, columns2);
    }
    return columns;
}

void mask_alleles(const vector<int>& consensus, alignment& A, const vector<pair<int,int>>& snp_lengths)
{
    int total = 0;
    for(int i=0;i<A.n_sequences();i++)
    {
        auto columns = get_allele_columns(consensus, A, i, snp_lengths);
        total += columns.size();
        mask_one_sequence(A, i, columns);
    }
    std::cerr<<"mask_alleles: masked a total of "<<total<<" columns, with an average of "<<double(total)/A.n_sequences()<<" per sequence.\n";
}

typedef vector<pair<int,int>> allele_t;

/*
 * How about cases where we have >=n snps in L bases, but n UNIQUE snps in L bases?  Does thus make sense if looking for a count of (say) 2 for each base?
 * Possibly, we could just pass min_count and max_count in to get_n_snps_versus consensus?
 */
vector<pair<allele_t,int>> find_alleles(const vector<int>& consensus, const alignment& A, int n_snps, int L_max, int min_count, int max_count, const vector<int>& seqs,
                                        const vector<int>& reference_positions)
{
    assert(consensus.size() == A.length());
    assert(n_snps > 1);
    assert(L_max > 1);
    assert(min_count > 0);

    // 1. Find alleles that satisfy n_snps SNPs in L_max bases, and record the count;

    std::map<allele_t, int> allele_count;
    for(int seq_index: seqs)
    {
        // Allele entries retain columns; window lengths use coordinates in sequence 0.
        auto snps = get_snps_versus_consensus(A, consensus, seq_index);

        for(int snp_index=0; snp_index+n_snps <= (int)snps.size(); snp_index++)
        {
            int L = reference_positions[snps[snp_index+n_snps-1].first]
                  - reference_positions[snps[snp_index].first] + 1;

            if (L > L_max) continue;

            allele_t allele(n_snps);
            for(int j=0;j<n_snps;j++)
                allele[j] = snps[snp_index+j];

            if (allele_count.count(allele))
                allele_count[allele] += 1;
            else
                allele_count[allele] = 1;
        }
    }

    // 2. Sort alleles by their genomic position
    vector<pair<allele_t,int>> alleles;
    for(auto& ac : allele_count)
        if (ac.second >= min_count and ac.second <= max_count)
            alleles.push_back(ac);

    std::ranges::sort(alleles, {}, [](auto& x){return x.first[0].first;});

    return alleles;
}

void show_alleles(const vector<pair<allele_t,int>>& alleles, const alphabet& a, const vector<int>& consensus,
                  const vector<int>& reference_positions)
{
    // 3. 
    std::cout<<"count"<<"\t"<<"start"<<"\t"<<"length"<<"\t"<<"consensus"<<"\t"<<"allele"<<"\n";
    for(auto& [allele, count]: alleles)
    {
        // Add 1 to the position, because it is stored as 0-based.
        int start = reference_positions[allele[0].first]+1;
        int length = reference_positions[allele.back().first] - reference_positions[allele[0].first]+1;
        string allele_chars;
        string consensus_chars;
        for(auto& [pos,c]: allele)
        {
            assert(a.is_letter(c));
            allele_chars += a.lookup(c);
            auto c2 = consensus[pos];
            consensus_chars += a.lookup(c2);
        }
        std::cout<<count<<"\t"<<start<<"\t"<<length<<"\t"<<consensus_chars<<"\t"<<allele_chars<<"\n";
    }
}

void translate_vcf(const string& vcf_filename, const string& alignments_filename)
{
    auto tables = get_translation_tables(alignments_filename);

    istream_or_ifstream vcf_file(std::cin, "-", vcf_filename);
    string line;
    set<string> unknown;
    string chrom;
    const translation_table* table = nullptr;

    while(portable_getline(vcf_file,line))
    {
        if (line.empty() or line[0] == '#')
        {
            std::cout<<line<<"\n";
            continue;
        }
        auto fields = split(line,'\t');
        if (fields.size() < 2)
            throw myexception()<<"VCF record has no position: "<<line;
        if (fields[0] != chrom or chrom.empty())
        {
            chrom = fields[0];
            auto it = tables.find(chrom);

            if (it == tables.end())
            {
                if(not unknown.count(chrom))
                {
                    unknown.insert(chrom);
                    std::cerr<<"No translation table for '"<<chrom<<"'\n";
                }
                table = nullptr;
            }
            else
                table = &(it->second);
        }

        if (table)
        {
            // Get zero-based coordinate from vcf 1-based coordinate
            int pos = convertTo<int>(fields[1])-1;
            if (pos < 0)
                throw myexception()<<chrom<<'\t'<<pos<<": should not be negative!";
            if (pos >= table->lookup.size())
                throw myexception()<<chrom<<'\t'<<pos<<": after end of chromosome of length "<<table->lookup.size();

            auto& [p1,p2]  = table->lookup[pos];
            if (p1 != p2)
            {
                std::cerr<<chrom<<'\t'<<pos<<": discarding record because it is missing in '"<<table->target_chrom<<"'\n";
                continue;
            }
            else if (log_verbose)
                std::cerr<<chrom<<'\t'<<pos<<": translating to "<<table->target_chrom<<"\t"<<p1<<"\n";

            fields[0] = table->target_chrom;
            // Get 1-based coordinate from 0-based coordinate
            fields[1] = std::to_string(p1+1);
            join(std::cout, fields, '\t');
            std::cout<<'\n';
        }
        else
            std::cout<<line<<'\n';
    }
}

void translate_mask(const string& mask_filename, const alignment& A)
{

    if (A.n_sequences() != 2)
        throw myexception()<<"translate-mask: expected exactly 2 sequences, but got "<<A.n_sequences()<<"!";

    // Source positions identify columns; target prefix counts identify sequence boundaries.
    // Their difference counts exactly the target bases inside a source interval's column span,
    // including internal insertions but excluding adjacent bases outside that span.
    vector<int> source_columns;
    vector<int> target_prefix(A.length()+1, 0);
    for(int c=0;c<A.length();c++)
    {
        if (not A.gap(c,0))
            source_columns.push_back(c);
        target_prefix[c+1] = target_prefix[c] + (not A.gap(c,1));
    }

    auto masks = read_masks(mask_filename);
    if (masks.size() == 0)
        throw myexception()<<"mask file is empty!";
    if (masks.size() > 1)
        throw myexception()<<"translate-mask: can only take 1 mask!";
                
    auto& mask = masks[0];
    std::cout<<">"<<A.seq(1).name<<"\n";
    for(const auto& [beg,end]: mask.intervals)
    {
        if (beg < 0 or end < beg or end >= source_columns.size())
            throw myexception()<<"Invalid 0-based inclusive source interval "<<beg<<" - "<<end
                               <<" for sequence of length "<<source_columns.size()<<".";

        int target_begin = target_prefix[source_columns[beg]];
        int target_end = target_prefix[source_columns[end]+1];
        if (target_begin < target_end)
            std::cout<<target_begin<<" - "<<target_end-1<<"\n";
    }
}


// Compute a reporting consensus and print qualifying alleles in reference coordinates.
static void report_alleles(const alignment& A, const string& consensus_range,
                           int n_snps, int length, int min_count, int max_count)
{
    auto consensus_seqs1 = parse_multi_range(consensus_range, A.n_sequences());
    auto consensus1 = get_consensus(A, consensus_seqs1);
    // Preserve reference numbering while retaining columns for allele identity and lookup.
    // A reference gap consumes no position and shares the next reference base's coordinate.
    vector<int> reference_positions(A.length());
    int ref_pos = 0;
    for(int c=0;c<A.length();c++)
    {
        reference_positions[c] = ref_pos;
        if (not A.gap(c,0)) ref_pos++;
    }
    auto alleles = find_alleles(consensus1, A, n_snps, length, min_count, max_count,
                                consensus_seqs1, reference_positions);
    show_alleles(alleles, A.get_alphabet(), consensus1, reference_positions);
}

// Report SNP distances for the existing sequence pairing and reference-coordinate convention.
static void write_snp_distances(const alignment& A, int max_d)
{
    // Go through non-overlapping pairs, using the first sequence in the pair as a reference.
    // (If we were scanning to remove things we would use every sequence as a reference, but this is just to get a distribution).
    std::cout<<"seq1\tseq2\tSNPs\tref_pos\tdistance\n";
    for(int i=0;i<A.n_sequences();i+=2)
    {
        int i2 = (i+1)%A.n_sequences();
        // positions are measured in terms of distance along i
        auto snp_positions = get_snp_positions(A,i,i2);
        for(int j=1;j<=max_d;j++)
            for(int k=0;k+j < snp_positions.size();k+=j)
                std::cout<<i<<"\t"<<i2<<"\t"<<j<<"\t"<<snp_positions[k].first<<"\t"<<snp_positions[k+j].second - snp_positions[k].second<<"\n";
    }

}

// Write selected sites either as an alignment or as the existing BED-style SNP table.
static void write_selected_sites(alignment& A, int count, double max_missing_freq,
                                 optional<int> width, const optional<string>& chromosome,
                                 bool show_frequencies)
{
    auto columns = find_columns(A, [&](int col) {
        return largest_minor_allele_count(A,col) >= count and missing_freq(A,col) <= max_missing_freq;
    });

    // Space out the kept columns - keep only 1 every width bases
    if (width)
    {
        auto bins = make_bins(columns, *width);
        columns.clear();
        for(auto& bin: bins)
            if (bin.size())
                columns.push_back(bin[0]);
    }

    if (chromosome)
    {
        if (show_frequencies)
            write_snp_bed_with_frequencies(std::cout, *chromosome, columns, A);
        else
            write_snp_bed(std::cout, *chromosome, columns, A);
    }
    else
    {
        select_columns_inplace(A, columns);
        std::cout<<A<<std::endl;
    }
}

// Print the existing scalar diagnostics after alignment or SMC output.
static void write_summary(const alignment& A, int original_length, int variant_dist)
{
    const auto& a = A.get_alphabet();
    //----- Count informative/non-constant sites ----//
    dynamic_bitset<> informative(A.length());
    dynamic_bitset<> different(A.length());

    dynamic_bitset<> alleles2(A.length());
    dynamic_bitset<> alleles3(A.length());
    dynamic_bitset<> is_masked(A.length());

    valarray<int> count(a.size());


    for(int c=0;c<A.length();c++) {
        count = 0;
        for(int i=0;i<A.n_sequences();i++) {
            int l = A(c,i);
            if (a.is_letter(l))
                count[l]++;

        }



        different[c]  =   is_informative(count ,0);
        informative[c]  = is_informative(count ,1);


        alleles2[c] = n_letters(count,0) == 2;
        alleles3[c] = n_letters(count,0) == 3;
        is_masked[c] = is_masked_column(A,c);
    }

    int n_different  = different.count();
    int n_informative  = informative.count();


    int non_masked_columns = A.length() - is_masked.count();

    if (A.length() != original_length)
        std::cerr<<"Length changed from "<<original_length<<" to "<<A.length()<<"\n";
    std::cerr<<"Masked "<<is_masked.count()<<" sites.  "<<A.length()-is_masked.count()<<" sites remain.\n";
    std::cerr<<"Sites with 2 alleles: "<<alleles2.count()<<" ("<<double(alleles2.count())/non_masked_columns<<")  ";
    std::cerr<<"Sites with 3 alleles: "<<alleles3.count()<<" ("<<double(alleles3.count())/non_masked_columns<<")  \n";
    {
        double pi_stat = pi(A);
        double theta = pi_stat/(1.0-pi_stat);
        std::cerr<<"pi = "<<pi_stat<<"    theta = "<<theta<<"\n";
    }
    std::cerr<<"Variant sites: "<<n_different<<" ("<<double(n_different)/non_masked_columns<<")  ";
    std::cerr<<"Variant sites at distance "<<variant_dist<<" before variant: "<<variant_column_at_distance(A,variant_dist).count()<<"\n";
    std::cerr<<"Informative sites: "<<n_informative<<" ("<<double(n_informative)/non_masked_columns<<")  \n";

}

// Apply the established preprocessing order before any analysis, including allele reporting.
static void apply_preprocessing(alignment& A, const preprocessing_options& options)
{
    if (options.reference)
        A = clean_to_ref(A, *options.reference);
    if (options.autoclean)
        autoclean(A);
    for(const auto& filename: options.mask_files)
        apply_masks(read_masks(filename), A);

    if (options.mask_gap_distance)
    {
        // Gap columns may be removed; neighboring columns retain their positions but are masked.
        int gap_label = options.strip_gaps ? 2 : 1;
        auto gaps = gap_columns(A, gap_label, options.gap_fraction);
        gaps = diffuse(gaps, *options.mask_gap_distance);
        remove_and_mask_columns(A, [&gaps](int c){return gaps[c];});
    }

    if (not options.mask_allele_specs.empty())
    {
        // This range selects sequences for consensus, not alignment columns.
        auto consensus_seqs = parse_multi_range(options.consensus_seqs, A.n_sequences());
        auto consensus = get_consensus_strict(A, consensus_seqs);
        vector<pair<int,int>> snp_lengths;
        for(const auto& spec: options.mask_allele_specs)
        {
            auto fields = split(spec, ':');
            if (fields.size() != 2)
                throw myexception()<<"argument to --mask-alleles should have the form n:L, but got '"<<spec<<"'";
            snp_lengths.emplace_back(convertTo<int>(fields[0]), convertTo<int>(fields[1]));
        }
        mask_alleles(consensus, A, snp_lengths);
    }
}

int main(int argc,char* argv[])
{
    try {
        cerr.precision(10);
        cout.precision(10);

        CLI::App app{"Prepare alignments, report variation, or translate coordinates.", "alignment-smc"};
        app.require_subcommand(1, 1);
        app.get_formatter()->long_option_alignment_ratio(0.2f);

        auto* show = app.add_subcommand("show", "Write the processed alignment to standard output");
        auto* alleles = app.add_subcommand("find-alleles", "Report alleles differing from a consensus");
        auto* distances = app.add_subcommand("snp-distances", "Report distances between SNPs");
        auto* histogram = app.add_subcommand("histogram", "Report SNP counts in blocks");
        auto* sfs = app.add_subcommand("sfs2d", "Report a whole-alignment two-population frequency spectrum");
        auto* select_sites = app.add_subcommand("select-sites", "Write selected columns as an alignment or BED table");
        auto* pi = app.add_subcommand("pi-matrix", "Report pairwise diversity");
        auto* msmc = app.add_subcommand("msmc", "Write MSMC input");
        auto* psmc = app.add_subcommand("psmc", "Write PSMC input");
        auto* dical2 = app.add_subcommand("dical2", "Write DiCal2 input");
        auto* mask = app.add_subcommand("translate-mask", "Translate mask coordinates through a pairwise alignment");
        auto* vcf = app.add_subcommand("translate-vcf", "Translate VCF positions using aligned sequence pairs");

        // Exactly one command is selected, so the shared input/settings have one active binding.
        string alignment_file;
        preprocessing_options preprocessing;
        for(auto* command: {show, alleles, distances, histogram, sfs, select_sites, pi, msmc, psmc, dical2})
        {
            command->add_option("ALIGNMENT", alignment_file, "Input alignment; '-' reads standard input")
                ->required()->type_name("");
            add_preprocessing_options(*command, preprocessing);
        }

        int n_snps = 0, allele_length = 0, min_count = 1;
        optional<int> max_count;
        alleles->add_option("--snps", n_snps, "Number of consecutive SNPs")
            ->required()->check(CLI::Range(2, std::numeric_limits<int>::max()));
        alleles->add_option("--length", allele_length, "Maximum inclusive span in reference positions")
            ->required()->check(CLI::Range(2, std::numeric_limits<int>::max()));
        alleles->add_option("--min-count", min_count, "Minimum allele occurrence count")
            ->check(CLI::PositiveNumber)->capture_default_str();
        alleles->add_option("--max-count", max_count, "Maximum allele occurrence count (default: sequence count)");

        int max_snps = 0, block_size = 0;
        distances->add_option("MAX_SNPS", max_snps, "Maximum number of SNP intervals between endpoints")
            ->required()->type_name("");
        histogram->add_option("BLOCK_SIZE", block_size, "Number of positions per block")
            ->required()->check(CLI::PositiveNumber)->type_name("");

        string pop1_prefix, pop2_prefix, ancestor_prefix;
        sfs->add_option("POP1", pop1_prefix, "First population's sequence-name prefix")->required()->type_name("");
        sfs->add_option("POP2", pop2_prefix, "Second population's sequence-name prefix")->required()->type_name("");
        sfs->add_option("ANCESTOR", ancestor_prefix, "Ancestral sequences' name prefix")->required()->type_name("");

        int minor_count = 0;
        double missing_frequency = 1.0;
        optional<int> width;
        optional<string> chromosome;
        bool show_frequencies = false;
        select_sites->add_option("--minor-allele", minor_count, "Minimum minor-allele count")->required();
        select_sites->add_option("--missing-freq", missing_frequency, "Maximum missing frequency")
            ->capture_default_str();
        select_sites->add_option("--one-every", width, "Keep one column per interval of this size")
            ->check(CLI::PositiveNumber);
        select_sites->add_option("--write-bed", chromosome, "Write a BED table with this chromosome name");
        select_sites->add_flag("--show-freq", show_frequencies, "With --write-bed, include allele frequencies");

        int variant_distance = 1;
        for(auto* command: {show, msmc, psmc, dical2})
            command->add_option("--variant", variant_distance, "SNP separation used in summary diagnostics")
                ->capture_default_str();

        string mask_file, vcf_file, mask_alphabet;
        mask->add_option("ALIGNMENT", alignment_file, "Source/target alignment; '-' reads standard input")
            ->required()->type_name("");
        mask->add_option("MASK", mask_file, "Source mask file with 0-based inclusive intervals")
            ->required()->type_name("");
        mask->add_option("--alphabet", mask_alphabet, "Alphabet (default: inferred from alignment)");
        vcf->add_option("ALIGNMENTS", alignment_file, "Consecutive source/target pairs; '-' reads standard input")
            ->required()->type_name("");
        vcf->add_option("VCF", vcf_file, "VCF file; '-' reads standard input")->required()->type_name("");

        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        if (*vcf)
        {
            translate_vcf(vcf_file, alignment_file);
            return 0;
        }
        if (*mask)
        {
            auto A = chop_internal(load_alignment(alignment_file, mask_alphabet, false), true);
            translate_mask(mask_file, A);
            return 0;
        }

        auto A = chop_internal(load_alignment(alignment_file, preprocessing.alphabet_name, preprocessing.erase_empty),
                               not preprocessing.erase_empty);
        int original_length = A.length();
        apply_preprocessing(A, preprocessing);

        if (*alleles)
            report_alleles(A, preprocessing.consensus_seqs, n_snps, allele_length, min_count,
                           max_count.value_or(A.n_sequences()));
        else if (*distances)
            write_snp_distances(A, max_snps);
        else if (*histogram)
            write_histogram(std::cout, block_size, A);
        else if (*sfs)
        {
            auto pop1 = find_sequence_subset_with_prefix(A, pop1_prefix);
            auto pop2 = find_sequence_subset_with_prefix(A, pop2_prefix);
            auto anc = find_sequence_subset_with_prefix(A, ancestor_prefix);
            auto counts = compute_2d_sfs(A, pop1, pop2, anc);
            for(int i=0;i<counts.size1();i++)
                for(int j=0;j<counts.size2();j++)
                    std::cout<<i<<"\t"<<j<<"\t"<<counts(i,j)<<"\n";
        }
        else if (*select_sites)
            write_selected_sites(A, minor_count, missing_frequency, width, chromosome, show_frequencies);
        else if (*pi)
            print_matrix(std::cout, pi_matrix(A), '\t');
        else
        {
            if (*msmc)
                write_msmc(std::cout, A);
            else if (*psmc)
                write_psmc(std::cout, A, 0, 1);
            else if (*dical2)
                write_dical2(std::cout, A);
            else
                std::cout<<A;
            std::cout.flush();
            write_summary(A, original_length, variant_distance);
        }
    }
    catch (std::exception& e) {
        cerr<<"alignment-smc: Error! "<<e.what()<<endl;
        return 1;
    }
    return 0;
}
