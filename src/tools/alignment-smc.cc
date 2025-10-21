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
#include <fstream>
#include <map>
#include <string>
#include <regex>
#include "tree/tree.H"
#include "alignment/alignment.H"
#include "alignment/load.H"
#include "alignment/alignment-util.H"
#include "util/string/split.H"
#include "util/string/join.H"
#include "findroot.H"
#include "parsimony.H"
#include "statistics.H"
#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include "util/assert.hh"
#include "util/io.H"
#include "util/cmdline.H"
#include "util/mapping.H"
#include "util/log-level.H"
#include "util/string/convert.H"
#include "util/string/pred.H"
#include "util/string/strip.H"
#include "util/range.H"
#include "util/matrix.H"
#include "util/io/matrix.H"

#include "range/v3/all.hpp"

using std::vector;
using std::valarray;
using std::set;
using std::map;
using std::pair;
using std::string;
using std::endl;
using std::ostream;
using std::optional;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

struct sequence_mask
{
    // Name
    string region_name;
    // 0-based regions to mask out.
    vector<pair<int,int>> intervals;
};

template<typename T>
void add(vector<T>& v1,const vector<T>& v2) 
{
    for(int i=0;i<v1.size();i++)
        v1[i] += v2[i];
}


vector<int> find_triplet(const sequence& s,const string& triplet) 
{
    sequence s2 = s;
    s2.strip_gaps();

    vector<int> found(3,0);

    int pos=-1;
    while(pos=s.find(triplet,pos+1),pos != -1)
        found[pos%3]++;

    return found;
}

vector<int> find_triplet(const vector<sequence>& sequences,const string& triplet) 
{
    vector<int> found(3,0);
    for(auto& sequence: sequences)
        add(found, find_triplet(sequence,triplet) );
    return found;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description all("Allowed options");
    all.add_options()
        ("help,h", "produce help message")
        ("align", value<string>(),"file with sequences and initial alignment")
        ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons")
        ("erase-empty-columns,e","Remove columns with no characters (all gaps).")

        ("minor-allele",value<int>(),"Keep columns with minor-allele count >= <arg>")
        ("missing-freq",value<double>()->default_value(1.0),"Keep columns with missing frequency >= <arg>")
        ("one-every",value<int>(),"Keep only 1 column in each interval of size <arg>")

        ("mask-gaps,G",value<int>(),"Remove columns within <arg> columns of a gap")
        ("strip-gaps,S","Remove columns within <arg> columns of a gap")
        ("gap-fraction",value<double>()->default_value(0.01),"Fraction of sequences that need to have a gap")
        ("mask-file,M",value<vector<string>>()->composing(),"Apply mask-file")
        ("autoclean,A","Mask blocks with too many SNPs")

        ("dical2","Output file for DiCal2")
        ("msmc","Output file for MSMC")
        ("psmc","Output file for PSMC")
        ("write-bed",value<string>(),"Output BED file with chromosome name <arg>") // BED only works for --minor-allele!

        // parameters
        ("variant",value<int>()->default_value(1),"Is there a SNP at distance <arg> from SNP?") // statistic
        ("consensus-seqs",value<string>(), "sequences to use in consensus") // For find-alleles

        ("translate-mask",value<string>(),"Masks (CSV or @file)")
        ("translate-vcf",value<string>(),"Masks (CSV or @file)")
        ("show-freq", "Show allele frequencies")
        ("clean-to-ref",value<string>(),"Remove columns not in reference sequence <arg>")
        ("pi-matrix","Calculate pi for each pair of sequences")
        ("histogram",value<int>(),"Output SNP counts for blocks of arg bases")
        ("snp-snp-lengths",value<int>(),"Output counts of snp-snp\nlengths up to arg snps")
        ("sfs2d",value<string>(),"pop1:pop2:anc:window")
        ("find-alleles",value<string>(), "Find alleles with S snps in L bases and count >= N")
        ("mask-alleles,m",value<vector<string>>()->composing(), "Find alleles with S snps in L bases and count >= N")
        ;

    // positional options
    positional_options_description p;
    p.add("align", 1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
          options(all).positional(p).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Generate input for SMC programs.\n\n";
        cout<<"Usage: alignment-smc <alignment-file> [OPTIONS]\n\n";
        cout<<all<<"\n";
        cout<<"Examples:\n\n";
        cout<<"  To calculate some statistics:\n";
        cout<<"    % alignment-smc sequence.fasta > /dev/null\n";
        cout<<"\n";
        cout<<"  To write out SNPS with minor-allele count >=2 in BED format:\n";
        cout<<"    % alignment-smc sequences.fasta --minor-allele=2 --write-bed=chrom > snps.bed\n";
        exit(0);
    }

    return args;
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
//??    if (level == 0) assert(n > 0);
    return n>1;
}


double min_identity(const alignment& A,bool gaps_count)
{
    double identity = 1.0;
    for(int i=0;i<A.n_sequences();i++)
        for(int j=0;j<i;j++)
            identity = std::min(identity,fraction_identical(A,i,j,gaps_count));

    return identity;
}

unsigned letter_classes(const alignment& A) 
{
    const alphabet& a = A.get_alphabet();

    // Count the occurrence of the different letters
    unsigned count=0;
    for(int i=0;i<A.length();i++) {
        for(int j=0;j<A.n_sequences();j++) {
            if (alphabet::is_letter_class(A(i,j)) and not a.is_letter(A(i,j)))
                count++;
        }
    }

    return count;
}
struct gap {
    int start;
    int length;
    mutable int type;
    gap(int s,int l,int t):start(s),length(l),type(t) {}
};

bool operator==(const gap& i1,const gap& i2)
{
    return (i1.start== i2.start) and (i1.length == i2.length);
}

bool operator<(const gap& i1,const gap& i2)
{
    if (i1.start < i2.start)
        return true;
    if (i1.start > i2.start)
        return false;
    return (i1.length < i2.length);
}

vector<gap> find_gaps(const alignment& A,int t)
{
    vector<gap> gaps;
    bool prev_gap=false;
    for(int i=0;i<A.length();i++)
    {
        if (A.gap(i,t)) {
            if (prev_gap)
                gaps.back().length++;
            else
                gaps.push_back(gap(i,1,1));
            prev_gap = true;
        }
        else
            prev_gap=false;
    }
    return gaps;
}

unsigned n_insertions(const alignment& A,int start,int length)
{
    unsigned count=0;
    for(int j=0;j<A.n_sequences();j++) {
        bool found = false;
        for(int k=0;k<length and not found;k++)
            if (A.gap(start+k,j))
                found=true;
        if (not found)
            count++;
    }
    return count;
}

map<gap,unsigned> guess_indels(const alignment& A)
{
    map<gap,unsigned> gaps;
    for(int i=0;i<A.n_sequences();i++)
    {
        for(auto& g: find_gaps(A,i))
            gaps[g]++;
    }

    // flip deletions in many taxa to insertions in few taxa
    for(map<gap,unsigned>::iterator i=gaps.begin();i!=gaps.end();) 
    {
        const gap& g = i->first;
        unsigned ins_count = n_insertions(A,g.start,g.length);
        if (ins_count < i->second) {
            g.type = 2;
            i->second = ins_count;
        }

        // increment counter and remove current element if count==0.
        {
            map<gap,unsigned>::iterator j = i;
            i++;
            if (not j->second)
                gaps.erase(j);
        }
    }
    return gaps;
}

vector<pair<int,int>> columns_to_regions(const vector<int>& columns)
{
    vector<pair<int,int>> regions;

    if (columns.empty()) return regions;

    regions.push_back({columns[0],columns[0]+1});
    for(int i=1; i<columns.size();i++)
    {
        if (regions.back().second == columns[i])
            regions.back().second++;
        else
            regions.push_back({columns[i],columns[i]+1});
    }

    return regions;
}

dynamic_bitset<> diffuse(const dynamic_bitset<>& v, int d)
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

vector<int> diffuse(const vector<int>& v, int d, int /*label*/)
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
    auto& a = A.get_alphabet();
    int last_snp = -1;
    for(int i=0;i<A.length();i++)
    {
        if (is_masked_column(A,i))
        {
            last_snp = i;
            continue;
        }

        if (not is_variant_column(A,i)) continue;
        
        o<<"1\t"<<i<<"\t"<<(i-last_snp)<<"\t";
        for(int j=0;j<A.n_sequences();j++)
            o<<a.lookup(A(i,j));
        o<<"\n";

        last_snp = i;
    }
}

void write_psmc(std::ostream& o, const alignment& A, int x1, int x2)
{
    const auto& a = A.get_alphabet();
    constexpr int blocksize = 1000;
    o<<">1\n";
    for(int i=0;i<A.length()/blocksize;i++)
    {
        bool diff = false;
        for(int j=0;not diff and j<blocksize;j++)
        {
            int c = blocksize * i + j;

            int l1 = A(c,x1);
            int l2 = A(c,x2);

            if (not a.is_letter(l1) or not a.is_letter(l2)) continue;

            diff = (l1 != l2);
        }
        if (diff)
            o<<"K";
        else
            o<<"T";
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




vector<int> expand_intervals(const vector<pair<int,int>>& intervals)
{
    vector<int> seq;
    for(auto& I: intervals)
        for(int i=I.first;i<=I.second;i++)
            seq.push_back(i);
    return seq;
}

void mask_one_sequence(alignment& A, int i, const vector<int>& columns)
{
    for(int col: columns)
    {
        if (A.character(col,i))
            A.set_value(col, i, alphabet::not_gap);
    }
}

void mask_columns(alignment& A, const vector<int>& columns)
{
    for(int c: columns)
        mask_column(A,c);
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

ostream& write_bed(ostream& o, const vector<sequence_mask>& regions)
{
    for(auto& [chrom,intervals]: regions)
        for(auto& interval: intervals)
            o<<chrom<<'\t'<<interval.first<<'\t'<<interval.second+1<<'\n';
    return o;
}

// BED files are 0-indexed.
vector<sequence_mask> read_bed(const string& filename)
{
    // 1. Read fine lines
    checked_ifstream file(filename,"BED file");

    vector<sequence_mask> masks;

    auto lines = read_lines(file);
    lines = select(lines, [](auto& line){return not strip(line," \t").empty();});

    for(auto& line: lines)
    {
        auto fields = resplit(line, R"([ \t]+))");
        if (fields.size() < 3)
            throw myexception()<<"Line does not appear to have 3 fields:\n |"<<line<<"\n";
        if (masks.empty() or masks.back().region_name != fields[0])
            masks.push_back({fields[0],{}});
        int start = convertTo<int>(fields[1]);
        int end = convertTo<int>(fields[2]);
        masks.back().intervals.push_back({start,end-1});
    }

    write_bed(std::cerr, masks);

    return masks;
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

vector<int> allele_counts_with_gap(const alignment& A, int col)
{
    auto& a = A.get_alphabet();
    vector<int> counts(a.size()+2, 0);
    const int not_gap = a.size()-1;
    const int gap = a.size();
    for(int i=0;i<A.n_sequences();i++)
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
    vector<int> counts(a.size()+1, 0);
    const int not_gap = a.size()-1;
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
    const int not_gap = a.size()-1;
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
    const int not_gap = a.size()-1;
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

            consensus[col] = argmax(counts);
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

ostream& write_bed(ostream& o, const string& chromosome, const vector<pair<int,int>>& columns)
{
    for(auto& [start,end]: columns)
        o<<chromosome<<'\t'<<start<<'\t'<<end<<'\n';
    return o;
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
        if (A(column,i) == alphabet::not_gap) continue;

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

ostream& write_bed(ostream& o, const string& chromosome, const vector<int>& columns)
{
    return write_bed(o, chromosome, columns_to_regions(columns));
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

        // Map beginning to beginng
        lookup[0] = {0,0};
        // Map end to end
        lookup.back() = {loc2,loc2};

        lookup.push_back({loc2,loc2}); // Handle end that is 1 too large?
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
                           const vector<int>& anc,
                           int /* window_size */)
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

        bool missing_data = false;
        for(int j: pop12)
            if (A.gap(i,j))
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

vector<pair<int,int>> get_snps_versus_consensus(const alignment& A, const vector<int> consensus, int i)
{
    auto& a = A.get_alphabet();

    vector<pair<int,int>> positions;

//    int i_pos=0;
    int ref_pos=0;
    for(int c=0;c<A.length();c++)
    {
        int li = A(c,i);
        int lc = consensus[c];
        if (a.is_letter(li) and a.is_letter(lc) and li != lc)
            positions.push_back({ref_pos, li});

//        if (not A.gap(c,i))
//            i_pos++;
        if (not A.gap(c,0))
            ref_pos++;
    }

    return positions;
}

vector<int> get_diff_columns(const alignment& A, const vector<int> consensus, int i)
{
    auto& a = A.get_alphabet();

    vector<int> columns;

    for(int c=0;c<A.length();c++)
    {
        if (not a.consistent( A(c,i), consensus[c] ) )
            columns.push_back(c);
    }

    return columns;
}

/*
//using boost::hash_combine
template <class T>
inline void hash_combine(std::size_t& seed, T const& v)
{
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

namespace std
{
    template<typename T>
    struct hash<vector<T>>
    {
        typedef vector<T> argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const& in) const
        {
            size_t size = in.size();
            size_t seed = 0;
            for (size_t i = 0; i < size; i++)
                //Combine the hash of the current vector with the hashes of the previous ones
                hash_combine(seed, in[i]);
            return seed;
        }
    };

    template <class T1, class T2>
    struct hash<pair<T1,T2>>
    {
        std::size_t operator()(const pair<T1,T2>& p) const noexcept
        {
            return std::hash<T1>()(p.first) ^ std::hash<T2>()(p.second);
        }
    };
}
*/

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
vector<pair<allele_t,int>> find_alleles(const vector<int>& consensus, const alignment& A, int n_snps, int L_max, int min_count, int max_count, const vector<int>& seqs)
{
    assert(consensus.size() == A.length());
    assert(n_snps > 1);
    assert(L_max > 1);
    assert(min_count > 0);

    // 1. Find alleles that satisfy n_snps SNPs in L_max bases, and record the count;

    std::map<allele_t, int> allele_count;
    for(int seq_index: seqs)
    {
        // The position here is versus the reference (sequence 0).
        auto snps = get_snps_versus_consensus(A, consensus, seq_index);

        for(int snp_index=0; snp_index<(int)snps.size()-n_snps; snp_index++)
        {
            int L = snps[snp_index+n_snps-1].first - snps[snp_index].first;

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

    ranges::sort(alleles, {}, [](auto& x){return x.first[0].first;});

    return alleles;
}

void show_alleles(const vector<pair<allele_t,int>>& alleles, const alphabet& a, const vector<int>& consensus)
{
    // 3. 
    std::cout<<"count"<<"\t"<<"start"<<"\t"<<"length"<<"\t"<<"consensus"<<"\t"<<"allele"<<"\n";
    for(auto& [allele, count]: alleles)
    {
        // Add 1 to the position, because it is stored as 0-based.
        int start = allele[0].first+1;
        int length = allele.back().first - allele[0].first+1;
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

void translate_vcf(const string& vcf_filename, const variables_map& args)
{
    auto alignments_filename = get_arg<string>(args,"align");
    if (not alignments_filename)
        throw myexception()<<"No filename specified for alignments to translate coordinates";

    auto tables = get_translation_tables(*alignments_filename);

    istream_or_ifstream vcf_file(std::cin, "-", vcf_filename);
    string line;
    while(portable_getline(vcf_file,line) and line.size() and line[0] == '#')
        std::cout<<line<<"\n";

    set<string> unknown;
    string chrom;
    const translation_table* table;

    while(portable_getline(vcf_file,line))
    {
        auto fields = split(line,'\t');
        assert(fields.size());
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
            if (pos > table->lookup.size())
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

void translate_mask(const string& mask_filename, const variables_map& args)
{
    //----------- Load alignment ---------//
    alignment A = load_A(args,false);

    if (A.n_sequences() != 2)
        throw myexception()<<"translate-mask: expected exactly 2 sequences, but got "<<A.n_sequences()<<"!";

    vector<pair<int,int>> map;
    int loc2 = -1;
    for(int c=0;c<A.length();c++)
    {
        if (A.character(c,1)) loc2++;

        if (A.character(c,0))
        {
            if (A.character(c,1))
                map.push_back({loc2,loc2});
            else
                map.push_back({loc2,loc2+1});
        }
    }
    // Map beginning to beginng
    map[0] = {0,0};
    // Map end to end
    map.back() = {loc2,loc2};
    map.push_back({loc2,loc2}); // Handle end that is 1 too large?

    auto masks = read_masks(mask_filename);
    if (masks.size() == 0)
        throw myexception()<<"mask file is empty!";
    if (masks.size() > 1)
        throw myexception()<<"translate-mask: can only take 1 mask!";
                
    auto& mask = masks[0];
    std::cout<<">"<<A.seq(1).name<<"\n";
    for(auto& [beg,end]: mask.intervals)
    {
        if (beg < 0)
            throw myexception()<<"0-indexed interval should not have negative start offset!";
        if (end > map.size())
            throw myexception()<<"0-indexed interval should end before the length of the source chromosome! ("<<map.size()<<")";
        if (beg > end)
            throw myexception()<<"interval should not begin before it ends!";

        beg = map[beg].first;
        end = map[end].second;
        std::cout<<beg<<" - "<<end<<"\n";
    }
}


int main(int argc,char* argv[])
{ 
    try {
        cerr.precision(10);
        cout.precision(10);
    
        //---------- Parse command line  -------//
        variables_map args = parse_cmd_line(argc,argv);

        if (auto vcf_filename = get_arg<string>(args, "translate-vcf"))
        {
            translate_vcf(*vcf_filename, args);
            exit(0);
        }

        if (auto filename = get_arg<string>(args, "translate-mask"))
        {
            translate_mask(*filename, args);
            exit(0);
        }

        //----------- Load alignment ---------//
        bool erase_empty = false;
        if (args.count("erase-empty-columns"))
            erase_empty = true;
        alignment A0 = load_A(args, false, erase_empty);

        auto A = A0;
        const alphabet& a = A.get_alphabet();

        if (auto ref_name = get_arg<string>(args, "clean-to-ref"))
            A = clean_to_ref(A, *ref_name);

        if (args.count("autoclean"))
            autoclean(A);

        if (args.count("mask-file"))
        {
            for(auto& filename: args["mask-file"].as<vector<string>>())
            {
                auto masks = read_masks(filename);
                apply_masks(masks, A);
            }
        }

        if (args.count("mask-gaps"))
        {
            // 1. label gap columns with 2 to remove them.
            int gap_label = 1;
            if (args.count("strip-gaps"))
                gap_label = 2;
            double gap_fraction = args["gap-fraction"].as<double>();

            auto gaps = gap_columns(A, gap_label, gap_fraction);

            // 2. label nearby columns with '1' to mask them
            int mask_gap_dist = args["mask-gaps"].as<int>();
            gaps = diffuse(gaps, mask_gap_dist, 1);

            // 3. Alter the alignment
            remove_and_mask_columns(A, [&gaps](int c){return gaps[c];});
        }

        if (args.count("find-alleles"))
        {
            // This takes a range of SEQUENCES, not a range of columns.
            string range = args.count("consensus-seqs") ? args["consensus-seqs"].as<string>() : "-";
            auto consensus_seqs1 = parse_multi_range(range, A.n_sequences());
            auto consensus1 = get_consensus(A, consensus_seqs1);

            auto find_args = split(args["find-alleles"].as<string>(),':');
            if (find_args.size() < 4)
                throw myexception()<<"argument to --find-alleles only has "<<find_args.size()<<" colon-separate elements.";
            int n_snps = convertTo<int>(find_args[0]);
            int L = convertTo<int>(find_args[1]);
            int min_count = 0;
            if (not find_args[2].empty())
                min_count = convertTo<int>(find_args[2]);
            int max_count = A.n_sequences();
            if (not find_args[3].empty())
                max_count = convertTo<int>(find_args[3]);

            auto alleles = find_alleles(consensus1, A, n_snps, L, min_count, max_count, consensus_seqs1);
            show_alleles(alleles, A.get_alphabet(), consensus1);
            exit(0);
        }

        if (args.count("mask-alleles"))
        {
            // This takes a range of SEQUENCES, not a range of columns.
            string range = args.count("consensus-seqs") ? args["consensus-seqs"].as<string>() : "-";
            auto consensus_seqs1 = parse_multi_range(range, A.n_sequences());
            auto consensus1 = get_consensus_strict(A, consensus_seqs1);

            vector<pair<int,int>> snp_lengths;
            for(const string& allele_spec: args["mask-alleles"].as<vector<string>>())
            {
                // Construct arguments n_snps, L
                auto find_args = split(allele_spec,':');
                if (find_args.size() != 2)
                    throw myexception()<<"argument to --mask-alleles should have the form n:L, but got '"<<args["mask-alleles"].as<string>();
                int n_snps = convertTo<int>(find_args[0]);
                int L = convertTo<int>(find_args[1]);
                snp_lengths.push_back({n_snps, L});
            }

            mask_alleles(consensus1, A, snp_lengths);
        }


        if (args.count("snp-snp-lengths"))
        {
            int max_d = args["snp-snp-lengths"].as<int>();
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

            exit(0);
        }

        if (args.count("histogram"))
        {
            write_histogram(std::cout, args["histogram"].as<int>(), A);
            exit(0);
        }

        if (args.count("sfs2d"))
        {
            auto args2d = split(args["sfs2d"].as<string>(),":");
            if (args2d.size() != 4)
                throw myexception()<<"sfs2d: argument '"<<args["sfs2d"].as<string>()<<"' should have 4 colon-separated parts";

            auto pop1prefix = args2d[0];
            auto pop2prefix = args2d[1];
            auto ancprefix = args2d[2];
            auto window_size = convertTo<int>(args2d[3]);
            auto pop1 = find_sequence_subset_with_prefix(A, pop1prefix);
            auto pop2 = find_sequence_subset_with_prefix(A, pop2prefix);
            auto anc = find_sequence_subset_with_prefix(A, ancprefix);

            auto sfs_counts = compute_2d_sfs(A,pop1,pop2,anc,window_size);
            for(int i=0;i<sfs_counts.size1();i++)
                for(int j=0;j<sfs_counts.size2();j++)
                    std::cout<<i<<"\t"<<j<<"\t"<<sfs_counts(i,j)<<"\n";

            exit(0);
        }

        if (args.count("minor-allele"))
        {
            int count = args["minor-allele"].as<int>();

            double max_missing_freq = args["missing-freq"].as<double>();

            auto columns = find_columns(A, [&](int col) {
                return largest_minor_allele_count(A,col) >= count and missing_freq(A,col) <= max_missing_freq;
            });

            // Space out the kept columns - keep only 1 every width bases
            if (args.count("one-every"))
            {
                int width = args["one-every"].as<int>();
                auto bins = make_bins(columns, width);
                columns.clear();
                for(auto& bin: bins)
                    if (bin.size())
                        columns.push_back(bin[0]);
            }

            if (args.count("write-bed"))
            {
                auto chromosome = args["write-bed"].as<string>();
                if (args.count("show-freq"))
                    write_snp_bed_with_frequencies(std::cout, chromosome, columns, A);
                else
                    write_snp_bed(std::cout, chromosome, columns, A);
            }
            else
            {
                select_columns_inplace(A, columns);
                std::cout<<A<<std::endl;
            }
            exit(0);
        }

        //----- Count informative/non-constant sites ----//
        dynamic_bitset<> informative(A.length());
        dynamic_bitset<> informative2(A.length());
        dynamic_bitset<> different(A.length());
        dynamic_bitset<> different2(A.length());
        dynamic_bitset<> could_be_different(A.length());
        dynamic_bitset<> contains_a_gap(A.length());

        dynamic_bitset<> alleles2(A.length());
        dynamic_bitset<> alleles3(A.length());
        dynamic_bitset<> is_masked(A.length());
        
        valarray<int> count(a.size());
        valarray<int> count2(2);

//        int pi_count = 0;
//        int pi_total = 0;

        for(int c=0;c<A.length();c++) {
            count = 0;
            count2 = 0;
            for(int i=0;i<A.n_sequences();i++) {
                int l = A(c,i);
                if (a.is_letter(l))
                    count[l]++;

                if (a.is_feature(l))
                    count2[0]++;
                else if (l == alphabet::gap)
                    count2[1]++;
            }

//            int total_letters = count.sum();
//            pi_total += total_letters*(total_letters-1)/2;

//            for(int l1=0;l1<a.size();l1++)
//                for(int l2=0;l2<l1;l2++)
//                    pi_count += count[l1]*count[l2];

            different[c]  =   is_informative(count ,0);
            informative[c]  = is_informative(count ,1);

            contains_a_gap[c] = (count2[1]>0);
            different2[c] =   different[c] or contains_a_gap[c];
            informative2[c] = informative[c] or is_informative(count2,1);

            alleles2[c] = n_letters(count,0) == 2;
            alleles3[c] = n_letters(count,0) == 3;
            is_masked[c] = is_masked_column(A,c);
        }

        int n_different  = different.count();
//      int n_same = A.length() - n_different;
        int n_informative  = informative.count();

//      int n_different2 = different2.count();
//      int n_same2 = A.length() - n_different2;
//      int n_informative2 = informative2.count();
//      int n_with_gaps = contains_a_gap.count();

        int non_masked_columns = A.length() - is_masked.count();

        if (args.count("msmc"))
            write_msmc(std::cout, A);
        else if (args.count("psmc"))
            write_psmc(std::cout,A,0,1);
        else if (args.count("dical2"))
            write_dical2(std::cout,A);
        else if (args.count("ldhat"))
        {
        }
        else if (args.count("pi-matrix"))
        {
            print_matrix(std::cout, pi_matrix(A),'\t');
            exit(0);
        }
        else
            std::cout<<A;
        std::cout.flush();

        if (A.length() != A0.length())
            std::cerr<<"Length changed from "<<A0.length()<<" to "<<A.length()<<"\n";
        int variant_dist = args["variant"].as<int>();
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

        // Look at LD, look at whether adjacent SNPs are correlated, and over what distance.
        // Look at blocks with more than
        // MASK columns instead of REMOVING them if they are w/in some distance of gap;
    }
    catch (std::exception& e) {
        cerr<<"alignment-smc: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}

