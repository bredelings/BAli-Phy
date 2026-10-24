/*
  Copyright (C) 2005-2006,2008 Benjamin Redelings

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

#include <string>
#include <vector>
#include <CLI/CLI.hpp>

#include "util/myexception.hh"
#include "alignment/alignment.hh"

#include "sequence/triplets.hh"

#include "alignment/alignment-util.hh"
#include "tree/tree-util.hh"
#include "parsimony.hh"
#include "joint-A-T.hh"
#include "tree-align/n_indels.hh"

#include "dp/A2_states.hh"
using namespace A2;

using std::cout;
using std::vector;
using std::string;
using std::endl;


// If we could read back in the indel-model state we could
// estimate the number of indels in an indel block.
vector<unsigned> n_indels_with_phase(const alignment& A,const Tree& T, int b)
{
    vector<unsigned> indels(4,0);

    vector<int> pairwiseA = get_path(A, T.branch(b).target(), T.branch(b).source());

    int last_state = states::M;
    // Each emitted triplet is three nucleotides. Phase is measured from the sequence start,
    // which must be a codon boundary for these counts to describe a biological reading frame.
    int nucleotide_width = A.get_alphabet().width();

    for(unsigned i=0,i1=0,i2=0; i<pairwiseA.size(); i++) 
    {
        int current_state = pairwiseA[i];
    
        if (last_state != current_state)
            if ((current_state == states::G1) or (current_state == states::G2)) 
            {
                int phase1 = i1%3;
                int phase2 = i2%3;
                if (phase1 == phase2)
                    indels[phase1]++;
                else
                    indels[3]++;
            }

        last_state = current_state;

        if (current_state == states::M or current_state == states::G2)
            i1 += nucleotide_width;

        if (current_state == states::M or current_state == states::G1)
            i2 += nucleotide_width;
    }

    return indels;
}

template <typename T,typename U>
void add(vector<T>& v1,const vector<U>& v2)
{
    for(unsigned i=0;i<v1.size();i++)
        v1[i] += v2[i];
}


vector<unsigned> n_indels_with_phase(const alignment& A,const Tree& T)
{
    vector<unsigned> indels(4,0);
    for(unsigned b=0;b<T.n_branches();b++)
        add(indels,n_indels_with_phase(A,T,b));

    return indels;
}

int main(int argc,char* argv[]) { 
    try {
        CLI::App app{"Summarize parsimony and indel counts from joint alignment and tree samples.", "joint-parsimony"};
        app.usage("Usage: joint-parsimony ALIGNMENTS TREES [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string alignment_file, tree_file, alphabet_name;
        unsigned subsample = 10;
        app.add_option("ALIGNMENTS", alignment_file, "File of alignment samples")->required()->type_name("");
        app.add_option("TREES", tree_file, "File of corresponding tree samples")->required()->type_name("");
        app.add_option("--subsample", subsample, "Factor by which to subsample trees")->capture_default_str();
        app.add_option("--alphabet", alphabet_name, "Alphabet to use (e.g. Codons)");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        joint_A_T J = get_joint_A_T(alignment_file, tree_file, alphabet_name, true, subsample);

        for(auto& [A,T]: J)
        {
            cout<<" length = "<<n_mutations(A,T)<<"   ";
      
            cout<<" #indels = "<<n_indels(A,T)<<"   ";

            cout<<" |indels| = "<<total_length_indels(A,T)<<"   ";

            const auto& a = A.get_alphabet();
            if (dynamic_cast<const Nucleotides*>(&a) or dynamic_cast<const Triplets*>(&a))
            {
                vector<unsigned> indels = n_indels_with_phase(A,T);
                for(int i=0;i<3;i++)
                    cout<<" phase"<<i<<" = "<<indels[i];
                cout<<" no-phase = "<<indels[3];
            }
            cout<<endl;
        }
    }
    catch (std::exception& e) {
        std::cerr<<"joint-parsimony: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
