#include "sequence-format.H"
#include "util.H"


namespace sequence_format {
  using std::vector;
  using std::string;

  vector<sequence> load_fasta(const alphabet& a,std::istream& file) {
    vector<sequence> sequences;

    string label;
    string letters;

    string line;
    while(getline(file,line)) {

      //quit on empty lines -- perhaps we should quit on TWO empty lines?
      //this allows us to put a sequence of alignments in a larger file
      if (!line.size()) break;

      if (line[0] == '>') {

	if (not label.empty()) {
	  sequence s(a);
	  s.parse(label,letters);
	  sequences.push_back(s);
	}
	
	label = line;
	letters.clear();
      }
      else
	letters += line;
	
    }

    if (not label.empty()) {
      sequence s(a);
      s.parse(label,letters);
      sequences.push_back(s);
    }

    return sequences;
  }

  vector<sequence> load_fasta(const alphabet& a,const string& filename) {
    ifstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Couldn't open file '"<<filename<<"'";
    vector<sequence> sequences = load_fasta(a,file);
    file.close();
    return sequences;
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
    getline(file,line);

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
  bool phylip_header_section(std::istream& file,int ntaxa, vector<string>& names,vector<string>& letters) {
    bool interleaved=true;

    names.clear();
    letters.clear();

    while(names.size() < ntaxa or not interleaved) {
      string name;
      string line_letters;

      bool empty_line = not phylip_header_line(file,name,line_letters,"");

      // parse line, and return false it empty;
      if (empty_line) break;

      // If the first line has no name, bail out
      if (not name.size() and names.size() == 0)
	throw myexception()<<"[Error reading PHYLIP alignment] First taxon has no name.";

      // If the second line has no name, assume non-interleaved
      if (not name.size() and names.size() == 1)
	interleaved = false;

      // If interleaved, assume that this is a new empty name.
      // If non-interleaved, lines w/o names go w/ the last name.
      if (name.size() or interleaved) {
	names.push_back(name);
	letters.push_back("");

	if (not name.size())
	  std::cerr<<"[Warning reading PHYLIP alignment]: taxon "<<names.size()+1<<" has an empty name!\n";
      }

      letters.back() += line_letters;
    }


    if (names.size() < ntaxa)
      throw myexception()<<"[Error reading PHYLIP alignment] Read an empty line after "<<names.size()<<" out of "<<ntaxa<<" sequences in the first stanza.";

    for(int i=1;i<letters.size();i++) 
      if (letters[i].size() != letters[0].size())
	throw myexception()<<"[Error reading PHYLIP alignment] Sequence '"<<names[i]<<"' has only "<<letters[i].size()<<" out of "<<letters[0].size()<<"letters in the first stanza";

    return interleaved;
  }

  /// Read the second and following phylip sections - no names
  bool phylip_section(std::istream& file,int ntaxa,vector<string>& letters) {
    letters.clear();
    string line;
    for(int i=0;i<ntaxa;i++) {
      assert(file);
      getline(file,line);
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

  void read_phylip(std::istream& file,vector<string>& names,vector<string>& sequences) {

    // parse phylip header
    string line;
    getline(file,line);
    int ntaxa = -1;
    int length = -1;
    {
      std::istringstream linestream(line);
      linestream>>ntaxa;
      linestream>>length;
    }

    int stanza=1;

    // Get the letters and names from first section
    bool interleaved = phylip_header_section(file,ntaxa,names,sequences);

    if (interleaved) {
      // Get the letters from following sections
      vector<string> letters;
      while(length <= 0 or sequences[0].size() < length) {
	string line;
	
	// If there is not more data, then quit
	if (not file.good()) break;
	
	getline(file,line);
	
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
  }


  string get_label(const string& line) {

    /*------------ Delete '>' from label -----------*/
    assert(line[0] == '>');
    string label = line.substr(1);

    /*------Parse label with name and comments------*/
    int where = label.find_first_of(" \t");
    if (where != -1)
      label = label.substr(0,where);

    return label;
  }


  void read_fasta(std::istream& file,vector<string>& names,vector<string>& sequences) {

    string label;
    string letters;

    string line;
    while(getline(file,line)) {
      if (!line.size()) continue;

      if (line[0] != '>') {
	letters += line;
	continue;
      }
    
      if (not label.empty()) {
	names.push_back(label);

	// Strip out space characters from the letters
	letters = strip(letters," \t");
	sequences.push_back(letters);
      }
    
      label = get_label(line);
      letters.clear();
      continue;
    }

    if (not label.empty()) {
      names.push_back(label);
      sequences.push_back(letters);
    }
  }

  /// Read an alignments letters and names from a file in phylip format
  void write_phylip(std::ostream& file, std::vector<std::string>& names_in, std::vector<std::string>& sequences) {
    vector<string> names = truncate_names(names_in);

    assert(names.size() == sequences.size());
    assert(names.size() > 0);


    const int nsequences = names.size();
    const int length = sequences[0].size();

    // Write header
    file<<nsequences<<" "<<length<<"\n";

    const int letters_length = 65;

    for(int pos=0;pos<length;pos += letters_length) {

      for(int seq = 0;seq < nsequences;seq++) {

	// get the line header (e.g. sequence name or spaces)
	string header = string(10,' ');
	if ((pos == 0) and (seq < nsequences)) {
	  string name = names[seq];
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
    file.flush();
  }

  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, std::vector<std::string>& names, std::vector<std::string>& sequences) {
    assert(names.size() == sequences.size());
    assert(names.size() > 0);

    const int letters_length = 70;

    for(int i=0;i<sequences.size();i++) {
      file<<">"<<names[i]<<"\n";
      for(int j=0;j<sequences[i].size();j+=letters_length)
	file<<sequences[i].substr(j,letters_length);
      file<<"\n";;
    }
    file.flush();
  }
}
