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
    result[i] = (int)(RGB[i]*256);
  return result;
}

string getrgb(const vector<int>& RGB) {
  string style = "rgb(";
  style += convertToString(RGB[0]) + ",";
  style += convertToString(RGB[1]) + ",";
  style += convertToString(RGB[2]) + ")";
  return style;
}

inline double f(double x) {
  //  x = x + 0.5*(x*x - x);
  x *= x;
  x *= 0.95;
  return x;
}

vector<int> rgb_bgcolor(double x, double sscale,bool color) {
  if (color) {
    double start = 0.71;
    double end = 0.0;
    
    double hstart = 0.4;
    double hend   = 0.95;
    
    double h = start + x * (end-start);
    double s = hstart + x * (hend - hstart);
    
    return hsv(h,s*sscale,1);
  }
  else
    return hsv(0,0,1.0 - f(x*sscale));
}

string getbgcolor(double x,double sscale,bool color) {
  return getrgb(rgb_bgcolor(x,sscale,color));
}

string getfgcolor(double x,double sscale,bool color) {
  if (color)
    return getrgb(hsv(0,0,0));
  else {
    if (1.0 - f(x*sscale) < 0.5)
      return getrgb(hsv(0,0,1));
    else
      return getrgb(hsv(0,0,0));
  }
}


string getstyle(double d,double sscale=1.0,bool color=true) {
  string style = string("background: ") + getbgcolor(d,sscale,color) + ";" ;
  style += "color: " + getfgcolor(d,sscale,color) + ";" ;
  return style;
}

string latex_rgb(const vector<int>& RGB) {
  string color = "{rgb}{";

  color += convertToString(double(RGB[0])/256.0) + ",";
  color += convertToString(double(RGB[1])/256.0) + ",";
  color += convertToString(double(RGB[2])/256.0) + "}";

  return string("c2");
  return color;
}


string latex_get_bgcolor(double d,double sscale=1.0,bool color=true) {
  return latex_rgb(rgb_bgcolor(d,sscale,color));
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

    bool columncolors = args.set("columncolors");

    bool show_column_numbers = args.set("positions");

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    vector<alphabet> alphabets;
    alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
    alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
    alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
    alignment A;
    A.load(alphabets,args["align"]);

    /*------ Link Alignment and Tree ----------*/
    link(A,T);

    /*------- Find mapping from colorfile to alignment sequence order -------*/
    if (not args.set("colors"))
      throw myexception("color file not specified! (colors=<filename>)");

    ifstream colorfile(args["colors"].c_str());

    vector<int> mapping;
    {
      string line;
      getline(colorfile,line);
      vector<string> colornames = split(line,' ');
      vector<string> leafnames = T.get_sequences();
      leafnames.erase(leafnames.begin()+T.leaves());
      mapping = compute_mapping(colornames,T.get_sequences());
    }

    // map the 
    mapping.push_back(mapping.size());

    /*------------------ Read in the colors ------------------------*/
    ublas::matrix<double> colors(A.length(),T.leaves()+1);
    for(int column=0;column<colors.size1();column++) 
      for(int i=0;i<colors.size2();i++) {
	double d;
	colorfile >> d;
	colors(column,mapping[i]) = d;
      }
	
    colorfile.close();

    /*-------------------- Get color or b/w ------------------*/
    bool color = true;
    if (args["color"] == "no")
      color = false;

    /*-------------------- Get width ------------------------*/
    int width =67;
    if (args.set("width"))
      width = convertTo<int>(args["width"]);

    /*-------------------- Get start ------------------------*/
    int start=0;
    if (args.set("start")) {
      start = convertTo<int>(args["start"]);
      if (start < 0)
	throw myexception()<<"Parameter 'start' must be positive.";
      if (not (start < A.length()))
	throw myexception()<<"Parameter 'start' must be less than the length of the alignment ("<<A.length()<<").";
    }
    
    /*-------------------- Get end ------------------------*/
    int end = A.length()-1;
    if (args.set("end")) {
      end = convertTo<int>(args["end"]);
      if (end < 0)
	throw myexception()<<"Parameter 'end' must be positive.";
      if (not (end < A.length()))
	throw myexception()<<"Parameter 'end' must be less than the length of the alignment ("<<A.length()<<").";
      if (end < start)
	throw myexception()<<"Parameter 'end' must be >= than parameter 'start'"<<A.length()<<").";
    }

    if (args["format"] == "latex") {
      std::cout<<"\\documentclass[10pt]{article}\n\
\\usepackage{colortbl}\n\
\\begin{document}\n\
\\definecolor{c2}{rgb}{1.0,0.5,0.0}\n";

      /*-------------------- Print the alignment ------------------------*/
      const alphabet& a = A.get_alphabet();
    
      int pos=start;
      while(pos<end) {
	cout<<"\\begin{tabular}{";
	for(int i=0;i<width;i++)
	  cout<<"c";
	cout<<"}"<<endl;
	
	for(int i=0;i<T.leaves();i++) {
	  int s = i;

	  for(int column=pos;column<pos+width and column < end; column++) {
	    char c = a.lookup(A(column,s));
	    double sscale=1.0;
	    if (A.gap(column,s))
	      sscale = 0.3;
	    string latexcolor = latex_get_bgcolor(colors(column,s),sscale,color);
	    if (column != pos)
	      cout<<"& ";
	    cout<<"\\multicolumn{1}{>{\\columncolor{"<<latexcolor<<"}}c}{"<<c<<"}"<<endl;
	  }
	  cout<<"\\\\"<<endl;
	}
	cout<<"\\end{tabular}"<<endl;
	pos += width;
      }
      
    }
    else {

      cout<<"\
<HTML>\n\
  <head>\n\
    <STYLE>\n\
TD { \n\
   font-size: 11pt; \n\
   padding: 0;\n\
   padding-right: 1em;\n\
   }\n\
\n\
TABLE {\n\
   border-spacing: 0\n\
   }\n\
\n\
SPAN {\n\
   font-family: courier new, courier-new, courier, monospace;\n\
   font-weight: bold;\n\
   font-size: 11pt;\n\
   line-height: 100%;\n\
   padding: 0;\n\
}\n\
    </STYLE>\n\
  </head>\n\
  <body>\n";
      
      /*-------------------- Print a legend ------------------------*/
      if (args["legend"] != "no") {
	cout<<"<P>From 0 to 1: ";
	const int nsquares = 20;
	for(int i = 0;i < nsquares+1;i++) {
	  double p = i*1.0/nsquares;
	  string style = getstyle(p,1,color);
	  cout<<"<span style=\""<<style<<"\">&nbsp;</span>";
	}
      }

      cout<<"<br><br>";

      /*----------- Compute the position headings -------------------*/
      string positions(A.length(),'.');

      positions[0]='1';
      for(int pos=10;pos<A.length();pos+=10) {
	string position = convertToString(pos);
	const int start = pos - position.size()+1;
	for(int j=0;j<position.size();j++) 
	  positions[start + j]=position[j];
      }


      /*-------------------- Print the alignment ------------------------*/
      const alphabet& a = A.get_alphabet();
    
      int pos=start;
      while(pos<end) {
	cout<<"<table>"<<endl;

	// Print columns positions
	if (show_column_numbers) {
	  cout<<"<tr><td></td><td>";
	  
	  for(int column=pos;column<pos+width and column < end; column++) {
	    double P=colors(column,T.leaves());
	    string style = getstyle(P,1.0,color);
	    if (columncolors)
	      cout<<"<span style=\""<<style<<"\">"<<positions[column]<<"</span>";
	    else
	      cout<<"<span>"<<positions[column]<<"</span>";
	  }
	  cout<<"</tr>\n";
	}

	for(int i=0;i<T.leaves();i++) {
	  int s = i;
	  cout<<"  <tr>"<<endl;
	  cout<<"    <td class=\"sequencename\">"<<T.seq(s)<<"</td>"<<endl;
	  cout<<"    <td>";
	  for(int column=pos;column<pos+width and column < end; column++) {
	    char c = a.lookup(A(column,s));
	    double sscale=1.0;
	    if (c == '-')
	      sscale = 0.3;
	    string style = getstyle(colors(column,s),sscale,color);
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
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
