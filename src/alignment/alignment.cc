/*
  Copyright (C) 2004-2009,2017 Benjamin Redelings

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

#include <algorithm>
#include <sstream>
#include "alignment.H"
#include "load.H"
#include "util/myexception.H"
#include "util/mapping.H"
#include "util/rng.H"
#include "util/range.H"
#include "util/string/convert.H"

using std::string;
using std::vector;
using std::endl;

namespace fs = std::filesystem;

void resize(matrix<int>& M1,int s1,int s2,int clear=0)
{
    matrix<int> M2(s1,s2);

    for(int i=0;i<M2.size1();i++)
	for(int j=0;j<M2.size2();j++)
	    M2(i,j) = clear;

    for(int i=0;i<M1.size1() and i<M2.size1();i++)
	for(int j=0;j< M1.size2() and j<M2.size2();j++)
	    M2(i,j) = M1(i,j);

    M1.swap(M2);
}

bool all_gaps(const alignment& A,int column,const boost::dynamic_bitset<>& mask) {
    for(int i=0;i<A.n_sequences();i++)
	if (mask[i] and A.character(column,i))
	    return false;
    return true;
}

bool all_gaps(const alignment& A,int column) {
    for(int i=0;i<A.n_sequences();i++)
	if (A.character(column,i))
	    return false;
    return true;
}

int n_characters(const alignment& A, int column) 
{
    int count=0;
    for(int i=0;i<A.n_sequences();i++)
	if (A.character(column,i))
	    count++;
    return count;
}


bool valid(const alignment& A) {
    for(int column=0;column<A.length();column++)
	if (all_gaps(A,column))
	    return false;
    return true;
}

void alignment::clear() {
    sequences.clear();
    array.resize(0,0);
}

int alignment::index(const string& s) const {
    for(int i=0;i<sequences.size();i++) 
	if (sequences[i].name == s) return i;

    return -1;
}

vector<int> alignment::get_columns_for_characters(int row) const
{
    vector<int> columns;

    columns.resize(length());
    int l=0;
    for(int c=0;c<length();c++)
	if (character(c,row))
	    columns[l++] = c;
    columns.resize(l);

    return columns;
}

std::string alignment::get_sequence_for_row(int row) const
{
    std::string seq;
    seq.reserve(length());
    for(int c=0;c<length();c++)
	if (character(c,row))
	    seq += a->lookup(array(c,row));
    return seq;
}

void alignment::changelength(int l) 
{
    array.resize(l,array.size2());
}

void alignment::delete_column(int column) {
    for(int i=0;i<n_sequences();i++) 
	assert(array(column,i) == alphabet::gap);

    matrix<int> array2(array.size1()-1,array.size2());
  
    for(int i=0;i<array2.size1();i++)
	for(int j=0;j<array2.size2();j++) {
	    int c = i;
	    if (c>=column) c++;
	    array2(i,j) = array(c,j);
	}
  
    array.swap(array2);
}

int alignment::seqlength(int i) const {
    int count =0;
    for(int column=0;column<length();column++) {
	if (character(column,i))
	    count++;
    }
    return count;
}

void alignment::add_row(const vector<int>& v) 
{
    int new_length = std::max(length(),(int)v.size());

    ::resize(array,new_length,n_sequences()+1,-1);

    for(int position=0;position<v.size();position++)
	array(position,array.size2()-1) = v[position];
}


void alignment::del_sequence(int ds) {
    assert(0 <= ds and ds < n_sequences());

    //----------- Fix the sequence list -------------//
    sequences.erase(sequences.begin()+ds);

    //-------------- Alter the matrix ---------------//
    matrix<int> array2(array.size1(),array.size2()-1);
  
    for(int i=0;i<array2.size1();i++)
	for(int j=0;j<array2.size2();j++) {
	    int s = j;
	    if (s>=ds) s++;
	    array2(i,j) = array(i,s);
	}
  
    array.swap(array2);
}

void alignment::del_sequences(const vector<int>& ds)
{
    vector<int> order = iota<int>(n_sequences());

    remove_elements(order, ds);

    (*this) = reorder_sequences(*this,order);
}

void alignment::add_sequence(const sequence& s) 
{
    add_row((*a)(s));

    sequences.push_back(s);
    sequences.back().strip_gaps();
}

void alignment::add_sequences(const vector<sequence>& S)
{
    // determine new length
    int new_length = length();
    for(int i=0;i<S.size();i++)
	new_length = std::max(new_length, (int)S[i].size());

    // resize the array
    int N = n_sequences();
    ::resize(array,new_length,n_sequences()+S.size(),-1);

    for(int i=0;i<S.size();i++)
    {
	sequences.push_back(S[i]);
	sequences.back().strip_gaps();

	for(int j=0; j<S[i].size(); j++)
	    array(j, N+i) = (*a)[S[i][j]];
    }
}

void alignment::load(const vector<sequence>& seqs) 
{
    // determine length
    unsigned new_length = 0;
    for(int i=0;i<seqs.size();i++) {
	unsigned length = seqs[i].size()/(*a).width();
	new_length = std::max(new_length,length);
    }

    // set the size of the array
    sequences.clear();
    array.resize(new_length,seqs.size());

    // Add the sequences to the alignment
    for(int i=0;i<seqs.size();i++)
    {
	try {
	    vector<int> v = (*a)(seqs[i]);
	    assert(v.size() <= array.size1());
	    int k=0;
	    for(;k<v.size();k++)
		array(k,i) = v[k];
	    for(;k<array.size1();k++)
		array(k,i) = alphabet::gap;
      
	    sequences.push_back(seqs[i]);
	    sequences.back().strip_gaps();
	}
	// Previously this was (catch bad_letter& e) - why so specific?
	catch (myexception& e)
	{
	    e.prepend("sequence #"+convertToString(i+1)+" '"+seqs[i].name+"':\n");
	    throw;
	}
    }

}

void alignment::load(const string& alph_name,const vector<sequence>& seqs)
{
    string full_name = guess_alphabet(alph_name, seqs);
    a = ::get_alphabet(full_name);
    load(seqs);
}

void alignment::load(sequence_format::loader_t loader,std::istream& file) 
{
    // read file
    vector<sequence> seqs = loader(file);

    // load sequences into alignment
    load(seqs);
}


void alignment::load(const string& alph_name, sequence_format::loader_t loader, std::istream& file) 
{
    // read file
    vector<sequence> seqs = loader(file);

    // load sequences into alignment
    load(alph_name,seqs);
}


string get_extension(const string& s) {
    int pos = s.rfind('.');
    if (pos == -1)
	return "";
    else
	return s.substr(pos);
}

void alignment::load(const fs::path& filename) 
{
    // read from file
    vector<sequence> seqs = sequence_format::load_from_file(filename);

    // load sequences into alignment
    load(seqs);
}

void alignment::load(const string& alph_name, const fs::path& filename)
{
    // read from file
    vector<sequence> seqs = sequence_format::load_from_file(filename);

    // load sequences into alignment
    load(alph_name,seqs);
}

void alignment::print_to_stream(std::ostream& file) const{
    const alphabet& a = get_alphabet();
    file<<length()<<endl;
    for(int start = 0;start<length();) {

	int end = start + 80;
	if (end > length()) end = length();

	for(int i=0;i<array.size2();i++) {
	    for(int column=start;column<end;column++) {
		file<<a.lookup(array(column,i));
	    }
	    file<<endl;
	}

	start = end;
	file<<endl<<endl;
    }
}

vector<sequence> alignment::convert_to_sequences() const 
{

    vector<sequence> seqs(n_sequences());

    for(int i=0;i<seqs.size();i++) {
	seqs[i].name = sequences[i].name;
	seqs[i].comment = sequences[i].comment;

	string& letters = seqs[i];
	letters = "";
	for(int c=0;c<length();c++)
	    letters += a->lookup( (*this)(c,i) );
    }

    return seqs;
}
vector<vector<int>> alignment::convert_to_letters() const
{
    // Reconstruct the list of letters
    vector<vector<int> > sequences;
    for(int i=0;i<n_sequences();i++) {
	vector<int> sequence;
	for(int c=0;c<length();c++) {
	    if (character(c,i))
		sequence.push_back((*this)(c,i));
	}
	sequences.push_back(sequence);
    }
    return sequences;
}

void alignment::write_sequences(sequence_format::dumper_t method,std::ostream& file) const {
    vector<sequence> seqs = convert_to_sequences();
    (*method)(file,seqs);
}

void alignment::print_fasta_to_stream(std::ostream& file) const {
    write_sequences(sequence_format::write_fasta,file);
}

void alignment::print_phylip_to_stream(std::ostream& file) const {
    write_sequences(sequence_format::write_phylip,file);
}

alignment::alignment(const alphabet& a1) 
    :a(a1.clone())
{}

alignment::alignment(const alphabet& a1, const vector<sequence>& S, int L) 
    :array(L,S.size()),sequences(S),a(a1.clone())
{
    // Do NOT load the sequences here -- this is used for setting the
    // sequences and matrix size and then filling in the matrix later.

    // This is used in reorder_sequences, among other things...
}

alignment::alignment(const alphabet& a1, int n, int L) 
    :array(L,n),sequences(n),a(a1.clone())
{
    // Do NOT load the sequences here -- this is used for setting the
    // sequences and matrix size and then filling in the matrix later.

    // This is used in compressing alignments (site-compression.{H,cc}, among other things...
}

alignment::alignment(const alphabet& a1,const fs::path& filename) 
    :a(a1.clone())
{ 
    load(filename); 

}

vector<int> get_path(const alignment& A,int node1, int node2) {
    vector<int> state;

    state.reserve(A.length()+1);
    for(int column=0;column<A.length();column++) {
	if (A.gap(column,node1)) {
	    if (A.gap(column,node2)) 
		continue;
	    else
		state.push_back(1);
	}
	else {
	    if (A.gap(column,node2))
		state.push_back(2);
	    else
		state.push_back(0);
	}
    }
  
    state.push_back(3);
    return state;
}


int remove_empty_columns(alignment& A) 
{
    int length = 0;

    for(int column=0;column<A.length();column++)
	if (not all_gaps(A,column)) 
	{
	    if (column != length)
		for(int i=0;i<A.n_sequences();i++)
		    A.set_value(length,i, A(column,i) );
	    length++;
	}

    int n_empty = A.length() - length;

    A.changelength(length);

    return n_empty;
}

std::ostream& operator<<(std::ostream& file,const alignment& A) 
{
    A.print_fasta_to_stream(file);
    return file;
}

std::istream& operator>>(std::istream& file,alignment& A) {
    A.load(sequence_format::read_fasta,file);
    return file;
}

vector<string> sequence_names(const alignment& A)
{
    return sequence_names(A,A.n_sequences());
}

vector<string> sequence_names(const alignment& A,int n)
{
    vector<string> names;

    for(int i=0;i<n;i++)
	names.push_back(A.seq(i).name);

    return names;
}

alignment blank_copy(const alignment& A1,int length) 
{
    alignment A2;

    // make an array w/ the same alphabet & sequences
    A2.a = A1.a;
    A2.sequences = A1.sequences;

    // make a blank array
    A2.array.resize(length, A1.array.size2());

    return A2;
}

alignment reorder_sequences(const alignment& A, const vector<int>& order)
{
    const unsigned L = A.length();

    vector<sequence> seqs;
    for(int j: order)
	seqs.push_back(A.seqs()[j]);

    alignment A2(A.get_alphabet(), seqs, L);

    for(int i=0;i<order.size();i++)
    {
	int j = order[i];
	assert(0 <= j and j < A.n_sequences());

	for(int c=0;c<L;c++)
	    A2.set_value(c,i, A(c,j) );
    }

    return A2;
}

alignment reorder_sequences(const alignment& A, const vector<string>& names)
{
    // Check the names and stuff.
    vector<string> n2 = sequence_names(A);

    if (names == n2) return A;

    try {
	vector<int> new_order = compute_mapping(names,n2);

	return reorder_sequences(A,new_order);
    }
    catch(bad_mapping<string>& e)
    {
	e.clear();
	if (e.size2 < e.size1)
	    e<<"Alignment has too few sequences! (Got "<<A.n_sequences()<<", expected "<<names.size()<<")\n";

	if (e.size1 < e.size2)
	    e<<"Alignment has too many sequences! (Got "<<A.n_sequences()<<", expected "<<names.size()<<")\n";

	if (e.from == 0)
	    e<<"  Alignment is missing sequence \""<<e.missing<<"\".";
	else
	    e<<"  Alignment has extra sequence \""<<e.missing<<"\".";
	throw;
    }
}

vector<int> get_sparse_alignment_row(const alignment& A, int i)
{
    vector<int> columns;
    for(int c=0;c<A.length();c++)
	if (A.character(c,i))
	    columns.push_back(c);
    return columns;
}

int homology_matrix::seqlength(int i) const
{
    int count =0;
    for(int column=0;column<length();column++) {
	if (character(column,i))
	    count++;
    }
    return count;
}

vector<int> homology_matrix::get_columns_for_characters(int row) const
{
    vector<int> columns;

    columns.resize(length());
    int l=0;
    for(int c=0;c<length();c++)
	if (character(c,row))
	    columns[l++] = c;
    columns.resize(l);

    return columns;
}

int sparse_increasing_index_matrix::length() const
{
    return L;
}

void sparse_increasing_index_matrix::finish_column()
{
    bool empty = true;
    for(int i=0;i<columns.size();i++)
	if (columns[i].size() and columns[i].back() == L)
	    empty = false;

    if (not empty)
	L++;
}

void sparse_increasing_index_matrix::set_present(int i)
{
    if (not columns[i].size() or columns[i].back() != L)
    {
	assert(not columns[i].size() or columns[i].back() < L);
	columns[i].push_back(L);
    }
}

sparse_increasing_index_matrix::sparse_increasing_index_matrix(int n)
    :L(0),columns(n)
{ }
