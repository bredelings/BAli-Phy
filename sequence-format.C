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
      if (!line.size()) continue;

      if (line[0] != '>') {
	letters += line;
	continue;
      }
    
      if (not label.empty()) {
	sequence s(a);
	s.parse(label,letters);
	sequences.push_back(s);
      }
    
      label = line;
      letters.clear();
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

  /// Read the first phylip section, including names
  bool phylip_section(std::istream& file,int ntaxa, vector<string>& names,vector<string>& letters) {
    names.clear();
    letters.clear();

    int indent = -1;
    for(int i=0;i<ntaxa;i++) {
      string line;

      assert(file);
      getline(file,line);
    
      if (not line.size())
	throw myexception()<<"[Reading PHYLIP alignment] Read an empty line after "<<i<<" out of "<<ntaxa<<" sequences in this stanza.";

      // Read the name from beginning of line
      int pos = line.find_first_not_of(" \t");
      if (pos == -1)
	throw myexception()<<"[Reading PHYLIP alignment] Expected "<<ntaxa<<" sequences, only found "<<i<<".";

      if (pos >9) // (note we're allowing empty names)
	names.push_back(string(""));
      else {
	int name_start = pos;
	pos = line.find_first_of(" \t",pos);
	names.push_back(line.substr(name_start,pos-name_start));
    
	pos = std::max(pos+1,10);
      }

      // find how much to indent
      pos = line.find_first_not_of(" \t",pos);
      assert(pos != -1);
      if (indent == -1)
	indent = pos;
      assert(indent == pos);
      line = line.substr(indent);

      // Strip out space characters from the letters
      line = strip(line," \t");

      // Add line to letters
      letters.push_back(line);
    }

    for(int i=1;i<letters.size();i++) 
      assert(letters[i].size() == letters[0].size());

    return file;
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
    phylip_section(file,ntaxa,names,sequences);

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
  void write_phylip(std::ostream& file, std::vector<std::string>& names, std::vector<std::string>& sequences) {
    assert(names.size() == sequences.size());
    assert(names.size() > 0);

    const int nsequences = names.size();
    const int length = sequences[0].size();

    // Write header
    file<<nsequences<<" "<<length<<endl;

    const int header_length = 10;
    const int letters_length = 65;

    for(int pos=0;pos<length;pos += letters_length) {

      for(int seq = 0;seq < nsequences;seq++) {

	// get the line header (e.g. sequence name or spaces)
	string header = string(header_length,' ');
	if ((pos == 0) and (seq < nsequences)) {
	  string name = names[seq];
	  if (name.size() > header.size())
	    name = name.substr(0,header.size());
	  
	  header = name + string(header_length-name.size(),' ');
	}
	
	// write out the line
	file<<header;
	file<<sequences[seq].substr(pos,letters_length);
	file<<endl;
      }
      // write one blank line;
      file<<endl;
    }
  }

  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, std::vector<std::string>& names, std::vector<std::string>& sequences) {
    assert(names.size() == sequences.size());
    assert(names.size() > 0);

    const int letters_length = 70;

    for(int i=0;i<sequences.size();i++) {
      file<<">"<<names[i]<<endl;
      for(int j=0;j<sequences[i].size();j+=letters_length)
	file<<sequences[i].substr(j,letters_length);
      file<<endl;
    }
  }
}
