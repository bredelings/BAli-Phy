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
#include "tree/tree.H"
#include "alignment/alignment.H"
#include "alignment/load.H"
#include "alignment/alignment-util.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"
#include "parsimony.H"
#include "statistics.H"
#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include "util/assert.hh"
#include "io.H"

using std::vector;
using std::valarray;
using std::map;
using std::pair;
using std::string;
using std::endl;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

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
    for(int i=0;i<sequences.size();i++)
	add(found, find_triplet(sequences[i],triplet) );
    return found;
}

vector<vector<pair<int,int>>> read_intervals_file(const string& filename)
{
    vector<string> lines = split(read_file(filename,"mask file"), '\n');
    vector<vector<pair<int,int>>> masks;
    for(const auto& line: lines)
    {
	if (startswith(line, ">"))
	    masks.push_back({});
	else
	{
	    auto x = split<int>(line," - ");
	    assert(x.size() == 2);
	    masks.back().push_back({x[0],x[1]});
	}
    }
    return masks;
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
	("strip-gaps,S","Remove columns within <arg> columns of a gap")
	("mask-gaps,G",value<int>()->default_value(0),"Remove columns within <arg> columns of a gap")
	("mask-file,M",value<vector<string>>()->composing(),"Apply mask-file")
	("variant",value<int>()->default_value(1),"Is there a SNP at distance <arg> from SNP?")
	("dical2","Output file for DiCal2")
	("msmc","Output file for MSMC")
	("psmc","Output file for PSMC")
	("autoclean","Mask blocks with too many SNPs")
	("histogram",value<int>(),"Output SNP counts for blocks of arg bases")
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
	exit(0);
    }

    return args;
}


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
	vector<gap> row = find_gaps(A,i);
	for(int j=0;j<row.size();j++)
	    gaps[row[j]]++;
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

bool is_masked_column(const alignment& A, int c)
{
    for(int i=0;i<A.n_sequences();i++)
	if (A(c,i) != alphabet::not_gap)
	    return false;
    return true;
}
bool is_variant_column(const alignment& A, int c)
{
    int i=0;
    int l0 = -1;
    for(;i<A.n_sequences() and l0 < 0;i++)
	l0 = A(c,i);

    for(;i<A.n_sequences();i++)
	if (A(c,i) >= 0 and A(c,i) != l0) return true;

    return false;
}

void remove_columns(alignment& A, const std::function<bool(int)>& remove)
{
    int j=0;
    for(int i=0;i<A.length();i++)
	if (not remove(i))
	{
	    // Copy column i -> column j
	    if (i != j)
		for(int k=0;k<A.n_sequences();k++)
		    A.set_value(j,k, A(i,k) );
	    j++;
	}
    A.changelength(j);
}


void mask_column(alignment& A, int column)
{
    for(int k=0;k<A.n_sequences();k++)
    {
	int value = A(column,k);
	if (alphabet::is_feature(value))
	    A.set_value(column,k, alphabet::not_gap);
    }
}

void remove_and_mask_columns(alignment& A, const std::function<int(int)>& remove_or_mask)
{
    int j=0;
    for(int i=0;i<A.length();i++)
    {
	int fate = remove_or_mask(i);

	// Remove this column
	if (fate == 2)
	    continue;

	// Mask the features in this column
	else if (fate == 1)
	    mask_column(A,i);

	// Copy column i -> j
	else if (i != 0)
	    for(int k=0;k<A.n_sequences();k++)
		A.set_value(j,k, A(i,k) );

	j++;
    }

    A.changelength(j);
}

dynamic_bitset<> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred)
{
    dynamic_bitset<> p(A.length());
    for(int i=0;i<A.length();i++)
	p[i] = pred(A,i);
    return p;
}

vector<int> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred, int label)
{
    vector<int> p(A.length(), 0);
    for(int i=0;i<A.length();i++)
	if (pred(A,i))
	    p[i] = label;
    return p;
}

dynamic_bitset<> gap_columns(const alignment& A)
{
    return find_columns(A, [](const alignment&A, int c) {return n_characters(A,c) != A.n_sequences();});
}

vector<int> gap_columns(const alignment& A, int label)
{
    return find_columns(A, [](const alignment&A, int c) {return n_characters(A,c) != A.n_sequences();}, label);
}

dynamic_bitset<> variant_columns(const alignment& A)
{
    return find_columns(A, is_variant_column);
}

dynamic_bitset<> variant_column_at_distance(const alignment& A,int d)
{
    auto variants = variant_columns(A);
    auto variants2 = variants;
    for(int i=0;i<A.length();i++)
    {
	if (not variants2[i]) continue;
	if (i-d <0 or not variants[i-d])
	    variants2[i] = false;
    }
    return variants2;
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


// The counts should be Poisson distributed.  It will be relatively rate to get
// counts more than 10 times higher than the mean

// FIXME - this does depend on the window boundaries.
// Change so that we use a sliding window.

int autoclean(alignment& A)
{
    constexpr double mean = 2.0;
    constexpr double factor = 10.0;
    int L = A.length();
    int S = n_snps(A);
    if (S < 100) throw myexception()<<"Refusing to autoclean chromosome with < 100 variant sites.";

    int masked = 0;
    int blocksize = int(L*(mean/S)+0.5);
    auto counts = snps_in_blocks(A, blocksize);

    for(int i=0;i<counts.size();i++)
	if (counts[i] >= mean*factor)
	{
	    masked++;
	    for(int j=i*blocksize;j<A.length() and j<(i+1)*blocksize;j++)
		mask_column(A, j);
	}
    return masked;
}


int main(int argc,char* argv[]) 
{ 
    try {
	cerr.precision(10);
	cout.precision(10);
    
	//---------- Parse command line  -------//
	variables_map args = parse_cmd_line(argc,argv);

	//----------- Load alignment and tree ---------//
	alignment A0 = load_A(args,false);

	auto A = A0;
	const alphabet& a = A.get_alphabet();
    
	if (args.count("autoclean"))
	    autoclean(A);

	if (args.count("histogram"))
	{
	    write_histogram(std::cout, args["histogram"].as<int>(), A);
	    exit(0);
	}

	if (args.count("mask-gaps"))
	{
	    // 1. label gap columns with 2 to remove them.
	    int gap_label = 1;
	    if (args.count("strip-gaps"))
		gap_label = 2;
	    auto gaps = gap_columns(A,gap_label);

	    // 2. label nearby columns with '1' to mask them
	    int mask_gap_dist = args["mask-gaps"].as<int>();
	    gaps = diffuse(gaps, mask_gap_dist, 1);

	    // 3. Alter the alignment
	    remove_and_mask_columns(A, [&gaps](int c){return gaps[c];});
	}

	//----- Count informative/non-constant sites ----//
	dynamic_bitset<> informative(A.length());
	dynamic_bitset<> informative2(A.length());
	dynamic_bitset<> different(A.length());
	dynamic_bitset<> different2(A.length());
	dynamic_bitset<> contains_a_gap(A.length());

	dynamic_bitset<> alleles2(A.length());
	dynamic_bitset<> alleles3(A.length());
	dynamic_bitset<> is_masked(A.length());
	
	valarray<int> count(a.size());
	valarray<int> count2(2);
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
//	int n_same = A.length() - n_different;
	int n_informative  = informative.count();

//	int n_different2 = different2.count();
//	int n_same2 = A.length() - n_different2;
//	int n_informative2 = informative2.count();
//	int n_with_gaps = contains_a_gap.count();

	int non_masked_columns = A.length() - is_masked.count();

	if (args.count("msmc"))
	    write_msmc(std::cout, A);
	else if (args.count("psmc"))
	    write_psmc(std::cout,A,0,1);
	else if (args.count("dical2"))
	    write_dical2(std::cout,A);
	else
	    std::cout<<A<<std::endl;

	if (A.length() != A0.length())
	    std::cerr<<"Length changed from "<<A0.length()<<" to "<<A.length()<<"\n";
	int variant_dist = args["variant"].as<int>();
	std::cerr<<"Masked "<<is_masked.count()<<" sites.\n";
	std::cerr<<"Sites with 2 alleles: "<<alleles2.count()<<" ("<<double(alleles2.count())/non_masked_columns<<")  ";
	std::cerr<<"Sites with 3 alleles: "<<alleles3.count()<<" ("<<double(alleles3.count())/non_masked_columns<<")  \n";
	std::cerr<<"Variant sites: "<<n_different<<" ("<<double(n_different)/non_masked_columns<<")  ";
	std::cerr<<"Variant sites at distance "<<variant_dist<<" before variant: "<<variant_column_at_distance(A,variant_dist).count()<<"\n";
	std::cerr<<"Informative sites: "<<n_informative<<" ("<<double(n_informative)/non_masked_columns<<")  \n";

	// Look at LD, look at whether adjacent SNPs are correlated, and over what distance.
	// Look at blocks with more than
	// MASK columns instead of REMOVING them if they are w/in some distance of gap;
    }
    catch (std::exception& e) {
	cerr<<"alignment-info: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}

