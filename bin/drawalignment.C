#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "rng.H"
#include "util.H"
#include "setup.H"

using std::cout;
using std::cerr;
using std::endl;


vector<int> hsv(double h,double s,double v) {
  h *= 6;

  int i = (int)h;
  double f = h-i;
  double p = v*(1-s);
  double q = v*(1-(s*f));
  double t = v*(1 - (s * (1-f)));

  vector<double> RGB(3);
  if (i==0) {
    RGB[0] = v; RGB[1] = t; RGB[2] = p;
  }
  else if (i==1) {
    RGB[0] = q; RGB[1] = v; RGB[2] = p;
  }
  else if (i==2) {
    RGB[0] = p; RGB[1] = v; RGB[2] = t;
  }
  else if (i==3) {
    RGB[0] = p; RGB[1] = q; RGB[2] = v;
  }
  else if (i==4) {
    RGB[0] = t; RGB[1] = p; RGB[2] = v;
  }
  else if (i==5) {
    RGB[0] = v; RGB[1] = p; RGB[2] = q;
  }
  else
    std::abort();

  vector<int> result(3);
  for(int i=0;i<3;i++)
    result[i] = RGB[i]*256;
  return result;
}

vector<int> getcolor(double x,double sscale) {
  double start = 0.71;
  double end = 0.0;

  double hstart = 0.4;
  double hend   = 0.95;
    
  double h = start + x * (end-start);
  double s = hstart + x * (hend - hstart);

  return hsv(h,s*sscale,1);
}

string getstyle(double d,double sscale=1.0) {
  vector<int> RGB = getcolor(d,sscale);
  string style = "background: rgb(";
  style += convertToString(RGB[0]) + ",";
  style += convertToString(RGB[1]) + ",";
  style += convertToString(RGB[2]) + ")";

  return style;
}


double objective_function(const alignment& A,const tree& T,const vector<int>& mapping) {
  double total = 0;
  for(int i=0;i<T.leaves()-1;i++) {
    total += T.distance(mapping[i],mapping[i+1]);
  }
  return total;
}


vector<int> optimize_mapping(const alignment& A,const tree& T) {
  vector<int> mapping(T.leaves());
  for(int i=0;i<T.leaves();i++)
    mapping[i] = i;

  double y = objective_function(A,T,mapping);
  int iterations=0;
  while(1) {
    int i = myrandom(T.leaves());
    int j = myrandom(T.leaves()-1);
    if (j >=i) j++;
    
    vector<int> mapping2 = mapping;
    std::swap(mapping2[i],mapping2[j]);
    double y2 = objective_function(A,T,mapping2);

    if (y2 < y) {
      mapping = mapping2;
      y = y2;
      iterations=0;
    }
    else
      iterations++;

    if (iterations > 100) break;
  }
  return mapping;
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cerr<<"random seed = "<<seed<<endl<<endl;
    
    cerr.precision(10);
    cout.precision(10);
    
    SequenceTree T;
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    T.read(args["tree"]);

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    vector<alphabet> alphabets;
    alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
    alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
    alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
    std::ifstream afile(args["align"].c_str());    
    alignment A;
    A.load_phylip(alphabets,afile);
    afile.close();

    /*------ Link Alignment and Tree ----------*/
    link(A,T);

    /*------- Find mapping which puts nearby things together -------*/
    vector<int> mapping = optimize_mapping(A,T);

    for(int i=0;i<mapping.size();i++)
      cerr<<T.seq(mapping[i])<<" ";
    cerr<<std::endl;

    if (args.set("just_reorder")) {
      alignment A2;
      for(int i=0;i<T.leaves();i++) {
	sequence s(A.seq(mapping[i]));
	s.resize(A.length());
	for(int column=0;column<A.length();column++)
	  s[column] = A(column,mapping[i]);
	A2.add_sequence(s);
      }

      A2.print_phylip(std::cout,true);
      exit(1);
    }

    /*------------------ Read in the colors-- ----------------------*/
    if (not args.set("colors"))
      throw myexception("color file not specified! (colors=<filename>)");
    ublas::matrix<double> colors(A.length(),T.leaves());
    ifstream colorfile(args["colors"].c_str());
    for(int column=0;column<A.length();column++) 
      for(int i=0;i<T.leaves();i++) {
	double d;
	colorfile >> d;
	colors(column,i) = d;
      }
	
    colorfile.close();

    /*-------------------- Print Things Out ------------------------*/
    int pos=0;
    int width =67;
    if (args.set("width"))
      width = convertTo<int>(args["width"]);

    cout<<"\
<HTML>\n\
  <head>\n\
    <STYLE>\n\
TD{ font-size: 11pt }\n\
SPAN {\n\
   font-family: courier new, courier-new, courier, monospace;\n\
   font-weight: bold;\n\
   font-size: 11pt;\n\
   line-height: 100%;\n\
}\n\
    </STYLE>\n\
  </head>\n\
  <body>\n";

    /*-------------------- Print a legend ------------------------*/
    cout<<"<P>From 0 to 1: ";
    const int nsquares = 20;
    for(int i = 0;i < nsquares+1;i++) {
      double p = i*1.0/nsquares;
      string style = getstyle(p);
      cout<<"<span style=\""<<style<<"\">&nbsp;</span>";
    }

    cout<<"<br><br>";
    /*-------------------- Print the alignment ------------------------*/

    const alphabet& a = A.get_alphabet();
    while(pos<A.length()) {
      cout<<"<table>"<<endl;
      for(int i=0;i<T.leaves();i++) {
	int s = mapping[i];
	cout<<"  <tr>"<<endl;
	cout<<"    <td>"<<T.seq(s)<<"</td>"<<endl;
	cout<<"    <td>";
	for(int column=pos;column<pos+width and column < A.length();column++) {
	  char c = a.lookup(A(column,s));
	  double sscale=1.0;
	  if (c == '-')
	    sscale = 0.3;
	  string style = getstyle(colors(column,s),sscale);
	  cout<<"<span style=\""<<style<<"\">"<<c<<"</span>";
	}
	cout<<"    </td>";
	cout<<"  </tr>"<<endl;
      }
      cout<<"</table>"<<endl;
      cout<<"<P>"<<endl;
      pos += width;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
