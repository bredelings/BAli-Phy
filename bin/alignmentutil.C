#include "alignmentutil.H"
#include "myexception.H"

using std::vector;
using std::string;

bool match_tag(const string& line,const string& tag) {
  if (line.size() < tag.size())
    return false;

  return (line.substr(0,tag.size()) == tag);
}


vector<alignment> load_alignments(std::istream& ifile, const string& tag,
				  const vector<alphabet>& alphabets, int maxalignments) {
  vector<alignment> alignments;
  
  // we are using every 'skip-th' line
  int skip = 1;

  // for each line (nth is the line counter)
  string line;
  for(int nth=0;getline(ifile,line);) {
    
    if (not match_tag(line,tag)) continue;

    // READ the next alignments, if we match the tag
    alignment A;
    try {
      A.load_phylip(alphabets,ifile);
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error load alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }
    

    // strip out empty columns
    remove_empty_columns(A);

    // complain if there are no sequences in the alignment
    if (A.num_sequences() == 0) 
      throw myexception(string("Alignment didn't contain any sequences!"));
    
    // STORE the alignment if we're not going to skip it
    if (nth%skip ==0) alignments.push_back(A);

    // If there are too many alignments
    if (alignments.size() > 2*maxalignments) {
      // start skipping twice as many alignments
      skip *= 2;

      std::cerr<<"Went from "<<alignments.size();
      // Remove every other alignment
      for(int j = alignments.size()-1;j>=0;j-=2) {
	alignments.erase(alignments.begin()+j);
      }
      std::cerr<<" to "<<alignments.size()<<" alignments.\n";

    }

    // Increment the counter IF we saw an alignment
    nth++;
  }

  // If we have too many alignments
  if (alignments.size() > maxalignments) {
    assert(alignments.size() < maxalignments*2);

    // We have this many extra alignments
    const int extra = alignments.size() - maxalignments;

    // Remove this many alignments from the array
    std::cerr<<"Went from "<<alignments.size();
    for(int i=extra-1;i>=0;i--) {
      int j = int( double ( double(i)*(alignments.size()-1)/(extra-1) ) );
      alignments.erase(alignments.begin()+j);
    }
    std::cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}


alignment find_last_alignment(std::istream& ifile, const string& tag,
				       const vector<alphabet>& alphabets) {
  alignment A;
  
  // for each line (nth is the line counter)
  string line;
  while(getline(ifile,line)) {
    
    if (not match_tag(line,tag)) continue;

    // READ the next alignments, if we match the tag
    try {
      alignment A2;
      A2.load_phylip(alphabets,ifile);
      A = A2;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error load alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // strip out empty columns
    remove_empty_columns(A);
  }

  if (A.num_sequences() == 0) 
    throw myexception(string("Couldn't find any alignments w/ tag ") + tag);

  return A;
}
