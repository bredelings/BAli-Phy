#include "alignmentutil.H"
#include "myexception.H"
#include "sequence-format.H"
#include <list>

using std::vector;
using std::string;
using std::list;

bool match_tag(const string& line,const string& tag) {
  if (line.size() < tag.size())
    return false;

  return (line.substr(0,tag.size()) == tag);
}


list<alignment> load_alignments(std::istream& ifile, const string& tag,
				  const vector<OwnedPointer<alphabet> >& alphabets, int maxalignments) {
  list<alignment> alignments;
  
  // we are using every 'skip-th' alignment
  int skip = 1;

  alignment A;
  string line;
  for(int nth=0;getline(ifile,line);) {
    
    // Continue with the next line IF no alignment begins here
    if (not match_tag(line,tag)) continue;

    // Increment the counter SINCE we saw an alignment
    nth++;

    // Skip this alignment IF it isn't the right multiple
    if (nth%skip != 0) continue;

    // READ the next alignment
    try {
      if (not alignments.size())
	A.load(alphabets,sequence_format::read_guess,ifile);
      else 
	ifile>>A;
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // strip out empty columns
    remove_empty_columns(A);

    // complain if there are no sequences in the alignment
    if (A.num_sequences() == 0) 
      throw myexception(string("Alignment didn't contain any sequences!"));
    
    // STORE the alignment if we're not going to skip it
    alignments.push_back(A);

    // If there are too many alignments
    if (alignments.size() > 2*maxalignments) {
      // start skipping twice as many alignments
      skip *= 2;

      std::cerr<<"Went from "<<alignments.size();
      // Remove every other alignment
      for(typeof(alignments.begin()) loc =alignments.begin();loc!=alignments.end();) {
	typeof(loc) j = loc++;

	alignments.erase(j);

	if (loc == alignments.end()) 
	  break;
	else
	  loc++;
      }
	
      std::cerr<<" to "<<alignments.size()<<" alignments.\n";

    }

  }

  // If we have too many alignments
  if (alignments.size() > maxalignments) {
    assert(alignments.size() < maxalignments*2);

    // We have this many extra alignments
    const int extra = alignments.size() - maxalignments;

    // Remove this many alignments from the array
    std::cerr<<"Went from "<<alignments.size();

    vector<int> kill(extra);
    for(int i=0;i<kill.size();i++)
      kill[i] = int( double(i+0.5)*alignments.size()/extra);
    std::reverse(kill.begin(),kill.end());

    int i=0;
    for(typeof(alignments.begin()) loc = alignments.begin();loc!=alignments.end();i++) {
      if (i == kill.back()) {
	kill.pop_back();
	typeof(loc) j = loc++;
	alignments.erase(j);
      }
      else
	loc++;
    }
    assert(kill.empty());
    std::cerr<<" to "<<alignments.size()<<" alignments.\n";
  }

  return alignments;
}


alignment find_last_alignment(std::istream& ifile, const string& tag,
				       const vector<OwnedPointer<alphabet> >& alphabets) {
  alignment A;

  // for each line (nth is the line counter)
  string line;
  while(getline(ifile,line)) {
    
    if (not match_tag(line,tag)) continue;

    // READ the next alignments, if we match the tag
    try {
      alignment A2;
      A2.load(alphabets,sequence_format::read_guess,ifile);
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
