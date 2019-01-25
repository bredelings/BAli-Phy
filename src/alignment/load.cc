#include "alignment/load.H"

#include "alignment-util.H"
#include "util/io.H"
#include "util/mapping.H"

extern int log_verbose;

using std::istream;
using std::vector;
using std::string;
using std::list;
using std::cerr;
using std::endl;

using std::optional;
using boost::program_options::variables_map;

std::string get_alphabet_name(const boost::program_options::variables_map& args)
{
    string alph_name;
    if (args.count("alphabet"))
	alph_name = args["alphabet"].as<string>();
    return alph_name;
}

vector<sequence> load_sequences_with_range(const string& filename, const string& range)
{
    vector<sequence> sequences = sequence_format::load_from_file(filename);

    if (not range.empty())
	sequences = select(sequences, range);

    return sequences;
}

alignment load_alignment_with_range(const string& filename, const string& range, const string& alph_name)
{
    auto sequences = load_sequences_with_range(filename,range);

    alignment A;

    try
    {
	A.load(alph_name, sequences);
    }
    catch (myexception& e)
    {
	if (range.empty())
	    e.prepend("In file '"+filename+"': ");
	else
	    e.prepend("In file '"+filename+"' columns "+range+": ");
	throw;
    }
  
    int n_empty = remove_empty_columns(A);
    if (n_empty)
	if (log_verbose >= 1) cerr<<"Warning: removed "<<n_empty<<" empty columns from alignment '"<<filename<<"'!\n"<<endl;
  
    if (A.n_sequences() == 0)
	throw myexception()<<"Alignment file "<<filename<<" didn't contain any sequences!";

    return A;
}

alignment load_alignment(const vector<sequence>& sequences, const string& alph_name)
{
    alignment A;

    A.load(alph_name, sequences);

    int n_empty = remove_empty_columns(A);
    if (n_empty)
	if (log_verbose >= 1) cerr<<"Warning: removed "<<n_empty<<" empty columns from alignment!\n"<<endl;

    if (A.n_sequences() == 0)
	throw myexception()<<"Alignment doesn't contain any sequences!";

    return A;
}

alignment load_alignment(const string& filename, const string& alph_name)
{
    try
    {
	auto sequences = sequence_format::load_from_file(filename);

	return load_alignment(sequences, alph_name);
    }
    catch (myexception& e)
    {
	e.prepend("Reading alignment from file '"+filename+"': ");
	throw;
    }
}

/// Load an alignment from command line args "--align filename"
alignment load_A(const variables_map& args,bool keep_internal) 
{
    // ----- Try to load alignment ------ //
    if (not args.count("align")) 
	throw myexception("Alignment file not specified! (--align <filename>)");
  
    string filename = args["align"].as<string>();
    alignment A = load_alignment(filename, get_alphabet_name(args));

    if (not keep_internal)
	A = chop_internal(A);

    return A;
}


using std::vector;
using std::string;
using std::list;

istream& find_alignment(istream& ifile)
{
    string line;
    while (ifile and ifile.peek() != '>')
    {
	if (not portable_getline(ifile,line)) break;
    }

    return ifile;
}

istream& skip_alignment(istream& ifile)
{
    string line;
    do {
	portable_getline(ifile,line);
    } while (ifile and line.size());

    return ifile;
}

istream& find_and_skip_alignment(istream& ifile)
{
    if (find_alignment(ifile))
	skip_alignment(ifile);
    return ifile;
}

istream& find_and_skip_alignments(istream& ifile, int n)
{
    for(int i=0;i<n and ifile;i++)
	find_and_skip_alignment(ifile);
    return ifile;
}

vector<sequence> load_next_sequences(istream& ifile)
{
    if (not find_alignment(ifile))
	throw myexception()<<"No alignment found.\n";

    try
    {
	auto sequences = sequence_format::read_fasta(ifile);
	if (sequences.empty())
	    throw myexception(string("Alignment didn't contain any sequences!"));
	return sequences;
    }
    catch (std::exception& e)
    {
	throw myexception()<<"Error loading alignment.\n  Exception: "<<e.what()<<"\n";
    }
}

alignment load_next_alignment(istream& ifile, const string& alph_name)
{
    alignment A;
    A.load(alph_name, load_next_sequences(ifile));

    // strip out empty columns
    remove_empty_columns(A);

    return A;
}


alignment load_next_alignment(istream& ifile, const alphabet& a)
{
    alignment A(a);
    A.load(load_next_sequences(ifile));

    // strip out empty columns
    remove_empty_columns(A);

    return A;
}


alignment load_next_alignment(istream& ifile, const alphabet& a, const vector<string>& names)
{
    return reorder_sequences(load_next_alignment(ifile,a), names);
}


optional<vector<sequence>> find_load_next_sequences(istream& ifile)
{
    if (not find_alignment(ifile)) return {};
    return load_next_sequences(ifile);
}


optional<alignment> find_load_next_alignment(istream& ifile, const string& alph_name)
{
    if (not find_alignment(ifile)) return {};
    return load_next_alignment(ifile, alph_name);
}


optional<alignment> find_load_next_alignment(istream& ifile, const alphabet& a)
{
    if (not find_alignment(ifile)) return {};
    return load_next_alignment(ifile, a);
}

optional<alignment> find_load_next_alignment(istream& ifile, const alphabet& a, const vector<string>& names)
{
    if (not find_alignment(ifile)) return {};
    return load_next_alignment(ifile, a, names);
}

template <typename T>
void thin_by_half(list<T>& Ts)
{
    // Remove every other alignment
    for(auto loc = Ts.begin();loc!=Ts.end();) 
    {
	auto j = loc++; 

	Ts.erase(j);

	if (loc == Ts.end())  break;

	loc++;
    }
}

template <typename T>
bool thin_down_to(list<T>& Ts,int max)
{
    int total = Ts.size();
    if (total <= max or max == -1)  return false;

    assert(total <= max*2);

    // We have this many extra Ts
    const int extra = total - max;
  
    vector<int> kill(extra);
    for(int i=0;i<kill.size();i++)
	kill[i] = int( double(i+0.5)*total/extra);
    std::reverse(kill.begin(),kill.end());
  
    int i=0;
    for(auto loc = Ts.begin();loc!=Ts.end();i++) {
	if (i == kill.back()) {
	    kill.pop_back();
	    auto j = loc++;
	    Ts.erase(j);
	    total--;
	}
	else
	    loc++;
    }
    assert(kill.empty());
    return true;
}

void insert_and_maybe_thin(alignment t, list<alignment>& Ts, int max, int& subsample)
{
    Ts.push_back(std::move(t));

    // If there are too many alignments
    if (max != -1 and Ts.size() > 2*max)
    {
	// start skipping twice as many alignments
	subsample *= 2;

	if (log_verbose >= 1) cerr<<"Went from "<<Ts.size();
	thin_by_half(Ts);
	if (log_verbose >= 1) cerr<<" to "<<Ts.size()<<" alignments.\n";
    }
}

template <typename T>
void load_more(list<T>& Ts, 
	       std::function<optional<T>(void)> next,
	       std::function<void(int)> skip,
	       int max,
	       int subsample=1) 
{
    try {
	while(auto A = next())
	{
	    // add the T and thin if possible
	    insert_and_maybe_thin(*A, Ts, max, subsample);

	    // skip over Ts due to subsampling
	    skip(subsample-1);
	}
    }
    // If we had a problem reading elements, still do the thinning.
    catch (std::exception& e) {
	thin_down_to(Ts, max);

	throw;
    }
    thin_down_to(Ts, max);
}

void load_more_alignments(list<alignment>& alignments, istream& ifile, const vector<string>& names, 
			  const alphabet& a, int maxalignments, int subsample=1) 
{
    try {
	auto next = [&ifile,&names,&a] () {return find_load_next_alignment(ifile,a,names); };
	auto skip = [&ifile] (int skip) {find_and_skip_alignments(ifile, skip); };
	load_more<alignment>( alignments, next, skip, maxalignments, subsample );
    }
    // If we had a problem reading elements, still do the thinning.
    catch (std::exception& e) {
	if (alignments.empty()) throw;

	cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
	cerr<<"  Exception: "<<e.what()<<endl;
    }

}

// Names and alphabet supplied as argument
list<alignment> load_alignments(istream& ifile, const vector<string>& names, const alphabet& a, int skip, int maxalignments) 
{
    find_and_skip_alignments(ifile,skip);

    list<alignment> alignments;
    load_more_alignments(alignments,ifile,names,a,maxalignments);

    return alignments;
}


// Get names from first alignment
std::list<alignment> load_alignments(std::istream& ifile, const string& alph_name, int skip, int maxalignments)
{
    list<alignment> alignments;
  
    find_and_skip_alignments(ifile, skip);

    alignments.push_back(load_next_alignment(ifile, alph_name));

    // The actually first alignment might be freed later on, so we can't
    // rely on a reference to it's alphabet.
    auto& first = alignments.front();
    object_ptr<const alphabet> a = first.get_alphabet().clone();

    vector<string> names = sequence_names(alignments.front());

    load_more_alignments(alignments, ifile, names, *a, maxalignments);

    return alignments;
}

// Names supplied as argument
std::list<alignment> load_alignments(std::istream& ifile, const vector<string>& names, const string& alph_name, int skip, int maxalignments)
{
    list<alignment> alignments;
  
    find_and_skip_alignments(ifile, skip);

    alignments.push_back( reorder_sequences( load_next_alignment(ifile,alph_name), names) );

    load_more_alignments(alignments, ifile, names, alignments.front().get_alphabet(), maxalignments);

    return alignments;
}


vector<alignment> load_alignments(istream& ifile, const string& alph_name)
{
    vector<alignment> alignments;
  
    vector<string> n1;

    alignment A;
    try {
	while(ifile) {

	    // CHECK if an alignment begins here
	    if (ifile.peek() != '>') {
		string line;
		portable_getline(ifile,line);
		continue;
	    }
    
	    // READ the next alignment
	    if (alignments.empty()) {
		A.load(alph_name, sequence_format::read_fasta, ifile);
		n1 = sequence_names(A);
	    }
	    else 
		ifile>>A;

	    // STRIP out empty columns
	    remove_empty_columns(A);

	    // COMPLAIN if there are no sequences in the alignment
	    if (A.n_sequences() == 0) 
		throw myexception(string("Alignment didn't contain any sequences!"));
    
	    // Check the names and stuff.
	    vector<string> n2 = sequence_names(A);

	    if (n1 != n2) {
		// inverse of the mapping n2->n1
		vector<int> new_order = compute_mapping(n1,n2);
		A = reorder_sequences(A,new_order);
	    }

	    // STORE the alignment if we're not going to skip it
	    alignments.push_back(A);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
	std::cerr<<"  Exception: "<<e.what()<<endl;
    }

    if (log_verbose >= 1) std::cerr<<"Loaded "<<alignments.size()<<" alignments.\n";

    return alignments;
}

alignment find_first_alignment(std::istream& ifile, const string& alph_name)
{
    alignment A;

    // for each line (nth is the line counter)
    string line;
    while(ifile) {
    
	// CHECK if an alignment begins here
	if (ifile.peek() != '>') {
	    string line;
	    portable_getline(ifile,line);
	    continue;
	}
    
	try {
	    // read alignment into A
	    alignment A2;
	    A2.load(alph_name, sequence_format::read_fasta, ifile);
	    A = A2;

	    // strip out empty columns
	    remove_empty_columns(A);
	    break;
	}
	catch (std::exception& e) {
	    std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
	    std::cerr<<"  Exception: "<<e.what()<<endl;
	    break;
	}

    }

    if (A.n_sequences() == 0) 
	throw myexception()<<"No alignments found.";

    return A;
}

alignment find_last_alignment(std::istream& ifile, const string& alph_name)
{
    alignment A;

    // for each line (nth is the line counter)
    string line;
    while(ifile) {
    
	// CHECK if an alignment begins here
	if (ifile.peek() != '>') {
	    string line;
	    portable_getline(ifile,line);
	    continue;
	}
    
	try {
	    // read alignment into A
	    alignment A2;
	    A2.load(alph_name, sequence_format::read_fasta, ifile);
	    A = A2;

	    // strip out empty columns
	    remove_empty_columns(A);
	}
	catch (std::exception& e) {
	    std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
	    std::cerr<<"  Exception: "<<e.what()<<endl;
	    break;
	}
    }

    if (A.n_sequences() == 0) 
	throw myexception()<<"No alignments found.";

    return A;
}

