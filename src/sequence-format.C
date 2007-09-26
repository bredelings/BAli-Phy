#include <fstream>
#include "sequence-format.H"
#include "util.H"

using namespace std;

namespace sequence_format {

  sequence fasta_parse_header(const string& line)
  {
    //------------ Delete '>' from label -----------//
    assert(line[0] == '>');
    string label = line.substr(1);

    //------Parse label with name and comments------//
    string name = label;
    string comment = "";

    int where = label.find_first_of(" \t");
    if (where != -1) {
      name = label.substr(0,where);
      
      where = label.find_first_not_of(" \t",where+1);
      if (where != -1)
	comment =  label.substr(where);
    }
    
    return sequence(name,comment);
  }

  vector<sequence> read_fasta(std::istream& file) {

    if (not file)
      throw myexception()<<"Reading sequences: file read error";

    vector<sequence> sequences;

    string line;

    bool done = false;

    while(not done and getline_handle_dos(file,line)) {

      // quit reading after a blank line
      if (not line.size()) break;

      // compare if expectations are met...
      if (line[0] != '>') 
	throw myexception()<<"FASTA sequence doesn't start with '>'";

      // Parse the header
      sequence s = fasta_parse_header(line);

      // Parse the letters
      string& letters = s;

      while (file) {
	char c = file.get();

	// bail on EOF
	if (not file) { done = true; break;}

	file.putback(c);
	
	// finish this sequence before beginning of next sequence
	if (c == '>') break;

	// read the next line of letters
	getline_handle_dos(file,line);

	// quit reading after a blank line
	if (not line.size()) { done = true; break;}
	
	// add the letters in
	letters += line;
      }
      letters = strip(letters," \t");

      // Add the sequence to the list
      sequences.push_back(s);
    }

    return sequences;
  }

  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, const std::vector<sequence>& sequences) {
    assert(sequences.size() > 0);

    const int letters_length = 70;

    for(int i=0;i<sequences.size();i++) {
      file<<">"<<sequences[i].name<<"   "<<sequences[i].comment<<"\n";

      for(int j=0;j<sequences[i].size();j+=letters_length)
	file<<sequences[i].substr(j,letters_length);
      file<<"\n";;
    }
    // write one blank line;
    file<<"\n";
    file.flush();
  }

  string strip_begin_end(const string& s) {
    int start=0;
    for(;start<s.size();start++) {
      if (s[start] != ' ' and s[start] != '\t')
	break;
    }

    int end = s.size()-1;
    for(;end>=start;end--) {
      if (s[end] != ' ' and s[end] != '\t')
	break;
    }
    
    return s.substr(start,end-start+1);
  }

  // after "<<i<<" out of "<<ntaxa<<" sequences in the first stanza.";
  //
  bool phylip_header_line(std::istream& file,string& name,string& letters,const string& location) {
    if (not file) 
      throw myexception()<<"[Error reading PHYLIP alignment] File ends early!";

    string line;
    getline_handle_dos(file,line);

    if (not strip(line," \t").size())
      return false;
    
    // Read the name from beginning of line
    string header = line.substr(0,10);
    name = strip_begin_end(header);

    // Strip out space characters from the letters
    letters = line.substr(10,line.size()-10);
    letters = strip(letters," \t");

    return true;
  }

  /// Read the first phylip section, including names
  bool phylip_header_section(std::istream& file,int ntaxa, vector<sequence>& sequences) {
    bool interleaved=true;

    sequences.clear();

    while(sequences.size() < ntaxa or not interleaved) {
      string name;
      string line_letters;

      bool empty_line = not phylip_header_line(file,name,line_letters,"");

      // parse line, and return false it empty;
      if (empty_line) break;

      // If the first line has no name, bail out
      if (not name.size() and sequences.size() == 0)
	throw myexception()<<"[Error reading PHYLIP alignment] First taxon has no name.";

      // If the second line has no name, assume non-interleaved
      if (not name.size() and sequences.size() == 1)
	interleaved = false;

      // If interleaved, assume that this is a new empty name.
      // If non-interleaved, lines w/o names go w/ the last name.
      if (name.size() or interleaved) {
	sequences.push_back(sequence(name,""));

	if (not name.size())
	  std::cerr<<"[Warning reading PHYLIP alignment]: taxon "<<sequences.size()+1<<" has an empty name!\n";
      }

      sequences.back() += line_letters;
    }


    if (sequences.size() < ntaxa)
      throw myexception()<<"[Error reading PHYLIP alignment] Read an empty line after "<<sequences.size()<<" out of "<<ntaxa<<" sequences in the first stanza.";

    for(int i=1;i<sequences.size();i++) 
      if (sequences[i].size() != sequences[0].size())
	throw myexception()<<"[Error reading PHYLIP alignment] Sequence '"<<sequences[i].name<<"' has only "<<sequences[i].size()<<" out of "<<sequences[0].size()<<"letters in the first stanza";

    return interleaved;
  }

  /// Read the second and following phylip sections - no names
  bool phylip_section(std::istream& file,int ntaxa,vector<string>& letters) {
    letters.clear();
    string line;
    for(int i=0;i<ntaxa;i++) {
      assert(file);
      getline_handle_dos(file,line);
      if (not line.size())
	throw myexception()<<"[Reading PHYLIP alignment] Read an empty line after "<<i<<" out of "<<ntaxa<<" sequences in this stanza.";

      // Strip out space characters from the letters
      line = strip(line," \t");

      // Add line to letters
      letters.push_back(line);
    }

    for(int i=1;i<letters.size();i++) 
      assert(letters[i].size() == letters[0].size());

    return file.good();
  }

  vector<sequence> read_phylip(std::istream& file) {

    if (not file)
      throw myexception()<<"Reading sequences: file read error";

    // parse phylip header
    string line;
    getline_handle_dos(file,line);
    int ntaxa = -1;
    int length = -1;
    {
      std::istringstream linestream(line);
      linestream>>ntaxa;
      linestream>>length;
    }

    int stanza=1;

    vector<sequence> sequences;

    // Get the letters and names from first section
    bool interleaved = phylip_header_section(file,ntaxa,sequences);

    if (interleaved) {
      // Get the letters from following sections
      vector<string> letters;
      while(length <= 0 or sequences[0].size() < length) {
	string line;
	
	// If there is not more data, then quit
	if (not file.good()) break;
	
	getline_handle_dos(file,line);
	
	// If there is a line here, and we are still looking for data, it must be empty
	if (line.size() != 0) 
	  throw myexception()<< "[Reading PHYLIP aligment] Expected an empty line after stanza "<<stanza<<", but read the following line:\n  \""<<line<<"\".";
	
	// If there is not more data, then quit
	if (not file.good()) break;
	
	stanza++;
	if (not phylip_section(file,ntaxa,letters))
	  break;
	
	for(int i=0;i<ntaxa;i++)
	  sequences[i] += letters[i];
      }
    }

    // Check that the length matches the supplied length
    if (length > 0 and length != sequences[0].size())
      throw myexception()<<
	"Sequences have length "<<sequences[0].size()<<
	" instead of specified length "<<length<<".";

    return sequences;
  }


  /// Read an alignments letters and names from a file in phylip format
  void write_phylip(std::ostream& file, const std::vector<sequence>& sequences) {

    //    vector<string> names = truncate_names(names_in);

    assert(sequences.size() > 0);

    const int letters_length = 70;

    const int length = sequences[0].size();

    // Write header
    file<<sequences.size()<<" "<<length<<"\n";

    for(int pos=0;pos<length;pos += letters_length) {

      for(int seq = 0;seq < sequences.size() ;seq++) {

	// get the line header (e.g. sequence name or spaces)
	string header = string(10,' ');
	if (pos == 0) {
	  string name = sequences[seq].name;
	  if (name.size() > header.size())
	    name = name.substr(0,header.size());
	  
	  header = name + string(header.size()-name.size(),' ');
	}
	
	// write out the line
	file<<header;
	file<<" ";
	file<<sequences[seq].substr(pos,letters_length);
	file<<"\n";
      }
      // write one blank line;
      file<<"\n";
    }
    // write one blank line;
    file<<"\n";
    file.flush();
  }

  vector<sequence> read_guess(std::istream& file) 
  {
    if (file.peek() == '>')
      return read_fasta(file);
    else
      return read_phylip(file);
  }

  vector<sequence> load_from_file(loader_t loader,const string& filename) {
    ifstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Couldn't open file '"<<filename<<"'";
    vector<sequence> sequences = loader(file);
    file.close();
    return sequences;
  }

  string get_extension(const string& s) {
    int pos = s.rfind('.');
    if (pos == -1)
      return "";
    else
      return s.substr(pos);
  }

  vector<sequence> load_from_file(const string& filename) {
    
    loader_t *loader = read_guess;

    string extension = get_extension(filename);
    if (extension == ".phy")
      loader = read_phylip;
    else if (extension == ".fasta")
      loader = read_fasta;
    
    // read from file
    return load_from_file(loader,filename);
  }

  vector<sequence> write_to_file(dumper_t dumper,const vector<sequence>& sequences,
				 const string& filename) {
    ofstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Couldn't open file '"<<filename<<"'";
    dumper(file,sequences);
    file.close();
    return sequences;
  }

}
