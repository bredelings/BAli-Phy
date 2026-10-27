/*
  Copyright (C) 2004-2005,2007-2008 Benjamin Redelings

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
#include <string>
#include <map>
#include <list>
#include "util/myexception.hh"
#include "alignment/alignment.hh"
#include "alignment/load.hh"
#include "util/string/split.hh"
#include <CLI/CLI.hpp>


using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::map;
using std::list;

// Load retained alignments and check that their underlying sequences agree.
void do_setup(const string& alphabet_name, int skip, int maxalignments, list<alignment>& alignments) 
{
    //------------ Try to load alignments -----------//
    cerr<<"Loading alignments...";
    alignments = load_alignments(std::cin,alphabet_name,skip,maxalignments);
    cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
    if (not alignments.size())
	throw myexception()<<"Alignment sample is empty.";

    // The loader already matches sequence names and order to the first retained alignment.
    for (const auto& A: alignments)
        for (int i=0; i<A.n_sequences(); i++)
            if (static_cast<const string&>(A.seq(i)) != static_cast<const string&>(alignments.front().seq(i)))
                throw myexception()<<"Sequence '"<<A.seq(i).name<<"' differs between sampled alignments.";
}

bool after(int c1, int c2, const alignment& A,const vector<int>& nodes) {
    assert(nodes.size() == A.n_sequences());

    for(int i=0;i<nodes.size();i++) {
	bool p1 = not A.gap(c1,nodes[i]);
	bool p2 = not A.gap(c2,nodes[i]);
	if (p2 and not p1)
	    return true;
	if (p1 and not p2)
	    return false;
    }
    return false;
}

bool intersect(int c1, int c2, const alignment& A) {
    for(int i=0;i<A.n_sequences();i++) {
	if (not A.gap(c1,i) and not A.gap(c2,i))
	    return true;
    }
    return false;
}

vector<int> getorder(const alignment& A,int n1,int n2) {

    // Get node order
    vector<int> nodes;
    nodes.push_back(n1);
    nodes.push_back(n2);
    for(int i=0;i<A.n_sequences();i++)
	if (i != n1 and i != n2)
	    nodes.push_back(i);

    // Get starting column arrangement
    vector<int> columns;
    for(int column=0;column<A.length();column++)
	columns.push_back(column);

    //-------- Re-order unordered columns by AP order ---------//
    for(int i=0;i+1<columns.size();) {
	if (not intersect(columns[i],columns[i+1],A) and after(columns[i],columns[i+1],A,nodes)) {
	    std::swap(columns[i],columns[i+1]);
	    if (i>0) i--;
	}
	else
	    i++;
    }

    vector<int> bits;
    for(int i=0;i<columns.size();i++) {
	int column = columns[i];
	int b = 0;
	if (not A.gap(column,n1))
	    b |= (1<<0);
	if (not A.gap(column,n2))
	    b |= (1<<1);
	if (b)
	    bits.push_back(b);
    }

    return bits;
}

void getpath(const vector<int>& bits,vector<int>& cx, vector<int>& cy) {
    cx.push_back(0);
    cy.push_back(0);
    int x=0;
    int y=0;
    for(int i=0;i<bits.size();i++) {
	int b = bits[i];
	if (b&(1<<0))
	    x++;
	if (b&(1<<1))
	    y++;
	cx.push_back(x);
	cy.push_back(y);
    }
}

struct edge {
    int x1;
    int y1;
    int x2;
    int y2;
    edge(int a, int b, int c, int d):x1(a),y1(b),x2(c),y2(d) {}
};

struct edge_lessthan {
    bool operator()(const edge& e1,const edge& e2) const {
	if (e1.x1 != e2.x1)
	    return (e1.x1 < e2.x1);
	else if (e1.y1 != e2.y1)
	    return (e1.y1 < e2.y1);
	else if (e1.x2 != e2.x2)
	    return (e1.x2 < e2.x2);
	else 
	    return (e1.y2 < e2.y2);
    }
};

void write_truck_graph(std::ostream& ofile,const list<alignment>& alignments,int t1,int t2,
		       const vector<string>& taxa) 
{
    map<edge,int,edge_lessthan> edges;
	
    //---------- Count the edges ----------//
    for(const alignment& A: alignments) {

	int n1 = A.index(taxa[t1]);
	if (n1 == -1)
	    throw myexception(string("sequence ") + taxa[t1] + " not found.");

	int n2 = A.index(taxa[t2]);
	if (n2 == -1)
	    throw myexception(string("sequence ") + taxa[t2] + " not found.");

	vector<int> bits = getorder(A,n1,n2);
      
	vector<int> cx;
	vector<int> cy;
	getpath(bits,cx,cy);

	for(int j=0;j<cx.size()-1;j++) {
	    int x1 = cx[j];
	    int y1 = cy[j];

	    int x2 = cx[j+1];
	    int y2 = cy[j+1];

	    edges[edge(x1,y1,x2,y2)]++;
	}
    }
  
    //----------- Write out header ----------//
    ofile<<alignments.size()<<" "<<edges.size()<<" "<<taxa[t1]<<" "<<taxa[t2]<<endl;

    //----------- Write out edges and counts ----------//
    for(map<edge,int,edge_lessthan>::iterator here = edges.begin();
	here != edges.end();here++) {
	ofile<<here->first.x1<<" "
	     <<here->first.y1<<" "
	     <<here->first.x2<<" "
	     <<here->first.y2<<" "
	     <<here->second<<endl;
    }
}


int main(int argc,char* argv[]) 
{ 
    try {
	//---------- Parse command line  -------//
        CLI::App app{"Count pairwise alignment-path edges across sampled alignments.", "path-graph"};
        app.usage("Usage: path-graph --taxa NAMES [OPTIONS] < alignments-file");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string taxa_names, alphabet_name;
        int skip = 0, maxalignments = 1000;
        app.add_option("--taxa", taxa_names, "Colon-separated taxon names; report every pair")->required();
        app.add_option("--alphabet", alphabet_name, "Specify the alphabet (default: inferred from alignment)");
        app.add_option("--skip", skip, "Number of alignment samples to skip")
            ->check(CLI::NonNegativeNumber)->capture_default_str();
        app.add_option("--max-alignments", maxalignments,
                       "Maximum retained alignments (thin the sample; -1 retains all)")
            ->check(CLI::PositiveNumber | CLI::IsMember({-1}))->capture_default_str();
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (taxa_names.empty())
            throw myexception("No taxa specified [taxa=name1:name2:...]");

	vector<string> taxa = split(taxa_names,':');

	/*---------------- Load alignment and tree -----------------*/
	list<alignment> alignments;
	do_setup(alphabet_name,skip,maxalignments,alignments);

	cerr<<"Read "<<alignments.size()<<" alignments\n";

	/*-------- Print the probabilistic truck graph for each pair -------*/
	for(int t1=0;t1<taxa.size();t1++)
	    for(int t2=0;t2<t1;t2++)
		write_truck_graph(std::cout,alignments,t1,t2,taxa);
    }
    catch (std::exception& e) {
	cerr<<"path-graph: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
