/*
  Copyright (C) 2008-2010 Benjamin Redelings

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

#include <fstream>
#include <optional>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include "util/myexception.hh"
#include "alignment/alignment.hh"
#include "optimize.hh"
#include "findroot.hh"
#include "util/string/join.hh"
#include "util/range.hh"
#include "alignment/load.hh"
#include "alignment/alignment-util.hh"
#include "alignment/index-matrix.hh"
#include "distance-methods.hh"

#include <CLI/CLI.hpp>


using std::shared_ptr;
using std::string;
using std::vector;
using std::endl;

// FIXME - also show which COLUMNS are more that 99% conserved?

// With characters, you take i~j~k and split the column, then if i and j
// are in different columns, then cannot both be aligned to k.  But, if
// you have i~j~-, then i and j can be in different columns and both aligned
// to - in the third sequence.  Likewise with '?'.

// If we compute the certainty of +~- only based on the + nodes, then + will
// always be 100% aligned, which doesn't seem fair.  Also, if we suddenly switch
// to '?', then the remaining characters will become "better aligned".

// If we could represent sub-groups, then we wouldn't need a root.

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::list;
using namespace optimize;

int score(const matrix<int>& M1, const matrix<int>& M2, int c1, int c2)
{
    assert(M1.size2() == M2.size2());
    const int N = M1.size2();

    int total = 0;

    for(int i=0;i<N;i++) 
    {
	if (M1(c1,i) == alphabet::unknown or M2(c2,i) == alphabet::unknown)
	    continue;

	if (M1(c1,i) == alphabet::unknown or M2(c2,i) == alphabet::gap)
	    continue;

	if (M1(c1,i) == M2(c2,i))
	    total++;
    }

    return total;
}

alignment stretch(const alignment& A1, const vector<int>& columns,int fill)
{
    alignment A2 = A1;
    A2.changelength(columns.size());
    for(int c=0;c<A2.length();c++) {
	if (columns[c] == -1)
	    for(int i=0;i<A2.n_sequences();i++)
		A2.set_value(c,i, fill);
	else
	    for(int i=0;i<A2.n_sequences();i++)
		A2.set_value(c,i, A1(columns[c],i) );
    }
    return A2;
}

void align_alignments(const matrix<int>& M1, const matrix<int>& M2, vector<int>& columns1, vector<int>& columns2)
{
    //--------- Construct Forward Matrix ---------//

    matrix<int> F(M1.size1()+1, M2.size1()+1);

    for(int i=0;i<F.size1();i++)
	for(int j=0;j<F.size2();j++) {
	    F(i,j) = 0;
	    if (i>0)         F(i,j) = std::max(F(i,j),F(i-1,j  ));
	    if (j>0)         F(i,j) = std::max(F(i,j),F(i  ,j-1));
	    if (i>0 and j>0) F(i,j) = std::max(F(i,j),F(i-1,j-1)+score(M1,M2,i-1,j-1));
	}

    //-------------- Find best path --------------//

    int i=F.size1()-1;
    int j=F.size2()-1;

    vector<int> S(3);
    while (i>0 or j>0) 
    {
	if (i>0) 
	    S[0] = F(i-1,j);
	else
	    S[0] = -1;

	if (j>0)
	    S[1] = F(i,j-1);
	else
	    S[1] = -1;

	if (i>0 and j>0)
	    S[2] = F(i-1,j-1)+score(M1,M2,i-1,j-1);
	else
	    S[2] = -1;

	int C = argmax(S);

	// cerr<<i<<"  "<<j<<"   C="<<C<<"    "<<S[0]<<" "<<S[1]<<" "<<S[2]<<endl;

	if (C == 0) {
	    i--;
	    columns1.push_back(i);
	    columns2.push_back(-1);
	}
	else if (C == 1)
	{
	    j--;
	    columns1.push_back(-1);
	    columns2.push_back(j);
	}
	else {
	    assert(C==2);
	    i--;
	    j--;
	    columns1.push_back(i);
	    columns2.push_back(j);
	}
    }

    std::reverse(columns1.begin(),columns1.end());
    std::reverse(columns2.begin(),columns2.end());
}


int main(int argc,char* argv[]) 
{ 
    try {
	//---------- Parse command line  -------//
        CLI::App app{"Align two alignments for comparison.", "alignments-diff"};
        app.usage("Usage: alignments-diff [OPTIONS] ALIGNMENT1 ALIGNMENT2");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string filename1, filename2, alphabet_name, fill_name = "gap";
        std::optional<string> differences_file;
        bool merge = false, dual = false;
        int blocksize = 20, n_types = 1;
        app.add_option("ALIGNMENT1", filename1, "First alignment")->required()->type_name("");
        app.add_option("ALIGNMENT2", filename2, "Second alignment")->required()->type_name("");
        app.add_option("--alphabet", alphabet_name, "Sequence alphabet (default: infer from input)")->type_name("ALPHABET");
        app.add_flag("--merge", merge, "Stack the aligned alignments with a blank separator row");
        app.add_flag("--dual", dual, "Write the two aligned alignments separately");
        app.add_option("--fill", fill_name, "Fill inserted columns with gap or unknown")->capture_default_str();
        app.add_option("-d,--differences-file", differences_file, "Write merged alignment differences in AU format")
            ->type_name("FILE");
        app.add_option("--blocksize", blocksize, "Width of blocks of the same color")->capture_default_str();
        app.add_option("--classes", n_types, "Number of groups for non-matching characters")->capture_default_str();
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

	alignment A1 = load_alignment(filename1, alphabet_name);
	check_names_unique(A1);

	alignment A2 = load_alignment(filename2, alphabet_name);
	A2 = reorder_sequences(A2, sequence_names(A1));

	matrix<int> M1 = M(A1);
	matrix<int> M2 = M(A2);

	int fill = alphabet::gap;
	if (fill_name == "unknown")
	    fill = alphabet::unknown;

	vector<int> columns1;
	vector<int> columns2;
	align_alignments(M1, M2, columns1, columns2);

	//----------- Write out Results ----------//
	alignment A1b = stretch(A1,columns1,fill);
	alignment A2b = stretch(A2,columns2,fill);

	matrix<int> M1b = M(A1b);
	matrix<int> M2b = M(A2b);

	if (merge) 
	{
	    alignment O = A1b;
	    // add a blank row

	    O.add_sequence(sequence("",""));
	    for(int c=0;c<O.length();c++)
		O.set_value(c,O.n_sequences()-1, fill);

	    // append the second alignment as extra rows
	    vector<sequence> sequences = A2b.convert_to_sequences();
	    for(int i=0;i<A2b.n_sequences();i++) {
		sequence s = sequences[i];
		s.name += '2';
		O.add_sequence(s);
	    }

	    cout<<O<<endl;

	    if (differences_file) 
	    {
		const int N = A1b.n_sequences();

		//	cerr<<"N = "<<N<<"  O is "<<O.n_sequences()<<" x "<<O.length()<<endl;
		// construct difference matrix C in AU format
		matrix<int> D(O.length(), O.n_sequences()+1);
		for(int c=0;c<D.size1();c++) {
		    D(c,N)=0;
		    D(c,2*N+1)=0;
		    for(int j=0;j<N;j++) {
			if (M1b(c,j) == M2b(c,j) and M1b(c,j) >= 0)
			    D(c,j) = D(c,j+1+N)=1;
			else
			    D(c,j) = D(c,j+1+N)=0;
		    }
		}

		// write out header: sequence names
		string filename = *differences_file;
		std::ofstream d(filename);
		for(int i=0;i<O.n_sequences();i++) {
		    d<<O.seq(i).name;
		    if (i !=  O.n_sequences()-1)
			d<<" ";
		    else
			d<<endl;
		}

		// write out D matrix
		for(int c=0;c<D.size1();c++) 
		{
		    for(int j=0;j<D.size2();j++) 
		    {
			d<<D(c,j);
			if (j != D.size2()-1)
			    d<<" ";
			else
			    d<<endl;
		    }
		}
	    }
	}
	else if (dual) {
	    cout<<A1b<<endl<<endl;
	    cout<<A2b<<endl<<endl;
	}
	else
	{
	    // Add 1 column for the ruler
	    auto column_lookup2 = column_lookup(A2);

	    matrix<int> D(A1.length(), A1.n_sequences()+1, 0);
	    for(int i=0;i<columns1.size();i++)
	    {
		int c1 = columns1[i];
		int c2 = columns2[i];

		if (c1 == -1) continue;

		bool any_different = (c2 == -1);
		for(int k=0;k<A1.n_sequences();k++)
		{
		    // Ruler agreement includes gaps, even though only characters receive colors.
		    if (c2 != -1 and M1(c1,k) != M2(c2,k))
			any_different = true;

		    // 0. Skip if there's not a letter at A1(c1,k)
		    if (M1(c1,k) < 0) continue;
		    
		    // 1. color the letter in A1 according to its position in A2
		    int c12 = column_lookup2[k][M1(c1,k)];
		    int block = c12 / blocksize;
		    int type = block % n_types;
		    D(c1,k) = 1+type;

		    // 2. mark the letter as a match if its the same one in A2.
		    if (c2 != -1)
		    {
			if (M1(c1,k) == M2(c2,k))
			    D(c1,k) = 0;
			else
			    any_different = true;
		    }
		}
		
		// Mark the ruler as unchanged if the whole column is unchanged
		if (not any_different) D(c1, A1.n_sequences()) = 1.0;
	    }

	    // write out header: sequence names
	    cout<<join(sequence_names(A1),' ')<<"\n";

	    // write out D matrix
	    for(int c=0;c<D.size1();c++) 
	    {
		for(int j=0;j<D.size2();j++) 
		{
		    cout<<D(c,j);
		    cout<<((j+1==D.size2())?'\n':' ');
		}
	    }
      
	}
    }
    catch (std::exception& e) {
	std::cerr<<"alignments-diff: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
