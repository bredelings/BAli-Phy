#include <cmath>
#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "util.H"
#include "setup.H"

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

static vector<int> black = hsv(0,0,0);
static vector<int> white = hsv(0,0,1);

string rgb_to_csv(double R, double G, double B) {
  string style = "rgb(";
  style += convertToString(R*256) + ",";
  style += convertToString(G*256) + ",";
  style += convertToString(B*256) + ")";
  return style;
}

string rgb_to_csv(const vector<int>& RGB) {
  return rgb_to_csv(RGB[0],RGB[1],RGB[2]);
}

double identity(double x) {return x;}
double square(double x) {return x*x;}
double cube(double x) {return x*x*x;}

double LOD(double x) {return log(x) - log(1.0-x);}

/// A representation of a transformation of [0,1] onto [min,max]
struct Scale {
  double min;
  double max;
  double (*f)(double);

  double operator()(double p) const {
    double v = f(p);
    if (v < min) 
      return 0;
    else if (v > max) 
      return 1;
    else
      return (v-min)/(max-min);
  }

  Scale():f(NULL) {}

  Scale(double d1,double d2,double (*g)(double))
    :min(d1),max(d2),f(g)
  { }
};

// Make separate schemes for fg and bg color, so that we can separate them?

// But then, how can we determine one color in reference to the other color?



class ColorMap {
public:
  virtual ColorMap* clone() const=0;

  virtual vector<int> bg_color(double x,const string& s) const=0;
  virtual vector<int> fg_color(double x,const string& s) const=0;
};

struct BW_ColorMap: public ColorMap {
public:
  BW_ColorMap* clone() const {return new BW_ColorMap(*this);}

  vector<int> bg_color(double x,const string& s) const {
    return hsv(0,0,1.0-x);
  }

  vector<int> fg_color(double x,const string& s) const {
    if (x < 0.5)
      return black;
    else
      return white;
  }
};

struct Rainbow_ColorMap: public ColorMap {
public:
  Rainbow_ColorMap* clone() const {return new Rainbow_ColorMap(*this);}

  vector<int> bg_color(double x,const string& s) const {
    if ((s == "-") or (s == "---")) {
      double v_uncertain = 1.0;
      double v_certain   = 0.5;
      double value = v_uncertain + x*(v_certain - v_uncertain);

      return hsv(0,0,value);
    }
    else {
      double c_start = 0.75;  // certain - Red
      double c_end   = 0;     // certain - Blue
      double color = c_start + x*(c_end - c_start);


      double h_start = 0.0;  // uncertain - white
      double h_end = 0.95;   // certain   - color

      double hue = h_start + x*(h_end - h_start);

      return hsv(color,hue,0.95);
    }
  }

  vector<int> fg_color(double x,const string& s) const {
    if (x < 0.5)
      return black;
    else
      return white;
      
  }
};

class ColorScheme {
  Scale scale;
  OwnedPointer<ColorMap> color_map;
public:
  vector<int> bg_color(double x,const string& s) const {
    return color_map->bg_color(scale(x),s);
  }

  vector<int> fg_color(double x,const string& s) const {
    return color_map->fg_color(scale(x),s);
  }

  ColorScheme(const Scale& S, const ColorMap& CM)
    :scale(S),color_map(CM)
  { }
};


using std::cout;
using std::cerr;
using std::endl;



string getstyle(double d,const string& s,const ColorScheme& color_scheme) {
  string style = string("background: ") + rgb_to_csv(color_scheme.bg_color(d,s)) + ";" ;
  style += "color: " + rgb_to_csv(color_scheme.fg_color(d,s)) + ";" ;
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


ublas::matrix<double> read_alignment_certainty(const alignment& A, const SequenceTree& T,
				const char* filename) 
{
  ifstream colorfile(filename);

  vector<int> mapping;
  {
    string line;
    getline(colorfile,line);
    vector<string> colornames = split(line,' ');
    vector<string> leafnames = T.get_sequences();
    leafnames.erase(leafnames.begin()+T.n_leaves());
    mapping = compute_mapping(colornames,T.get_sequences());
  }

  // Add an entry for the ENTIRE COLUMN
  mapping.push_back(mapping.size());

  //------------------ Read in the colors ------------------------//
  ublas::matrix<double> colors(A.length(),T.n_leaves()+1);
  for(int column=0;column<colors.size1();column++) 
    //TODO - use an istringstream to make things properly per-line
    for(int i=0;i<colors.size2();i++) {
      double d;
      colorfile >> d;
      colors(column,mapping[i]) = d;
    }
  
  colorfile.close();

  return colors;
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    cerr.precision(10);
    cout.precision(10);
    
    bool columncolors = args.set("columncolors");

    bool show_column_numbers = args.set("positions");

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T);

    /*------- Find mapping from colorfile to alignment sequence order -------*/
    if (not args.set("colors"))
      throw myexception()<<"color file not specified! (colors=<filename>)";

    ublas::matrix<double> colors = read_alignment_certainty(A,T,args["colors"].c_str());

    /*-------------------- Get color or b/w ------------------*/
    OwnedPointer<ColorMap> color_map;

    if (args["color"] == "no" or args["color"] == "bw") 
      color_map = OwnedPointer<ColorMap>(new BW_ColorMap);
    else
      color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap);

    Scale scale;
    if (args["scale"] != "uniform") {
      scale.f = LOD;
      scale.min = args.loadvalue("min",-3);
      scale.max = args.loadvalue("max",3);
    }
    else {
      scale.f = identity;
      scale.min = args.loadvalue("min",0);
      scale.max = args.loadvalue("max",1);
    }

    ColorScheme color_scheme(scale,*color_map);

    double gapscale = args.loadvalue("gapscale",0.3);
    bool showgaps = true;
    if (args["showgaps"] == "no")
      showgaps = false;

    /*-------------------- Get width ------------------------*/
    int width =args.loadvalue("width",67);

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
	
	for(int i=0;i<T.n_leaves();i++) {
	  int s = i;

	  for(int column=pos;column<pos+width and column < end; column++) {
	    string c = a.lookup(A(column,s));
	    string latexcolor = "";//latex_get_bgcolor(colors(column,s),sscale,color);
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
   font-size: 9pt; \n\
   padding: 0;\n\
   padding-right: 1em;\n\
   }\n\
\n\
TD.sequencename {\n\
   font-style: italic;\n\
   font-family: helvitica;\n\
}\n\
TABLE {\n\
   border-spacing: 0;\n\
   margin-top: 0.4em;\n\
   }\n\
\n\
SPAN {\n\
   font-family: courier new, courier-new, courier, monospace;\n\
   font-weight: bold;\n\
   font-size: 10pt;\n\
   line-height: 100%;\n\
   padding: 0;\n\
}\n\
    </STYLE>\n\
  </head>\n\
  <body>\n";
      
      //-------------------- Print a legend ------------------------//
      if (args["legend"] != "no") {
	cout<<"<P>From 0 to 1: ";
	const int nsquares = 20;
	for(int i = 0;i < nsquares+1;i++) {
	  double p = i*1.0/nsquares;
	  string style = getstyle(p,"",color_scheme);
	  cout<<"<span style=\""<<style<<"\">&nbsp;</span>";
	}
      }

      cout<<"<br><br>";

      //----------- Compute the position headings -------------------//
      string positions(A.length(),'.');

      positions[0]='1';
      for(int pos=10;pos<A.length();pos+=10) {
	string position = convertToString(pos);
	const int start = pos - position.size()+1;
	for(int j=0;j<position.size();j++) 
	  positions[start + j]=position[j];
      }


      //-------------------- Print the alignment ------------------------//
      const alphabet& a = A.get_alphabet();
    
      int pos=start;
      while(pos<end) {
	cout<<"<table>"<<endl;

	// Print columns positions
	if (show_column_numbers) {
	  cout<<"<tr><td></td><td>";
	  
	  for(int column=pos;column<pos+width and column < end; column++) {
	    double P=colors(column,T.n_leaves());
	    string style = getstyle(P,"",color_scheme);
	    if (columncolors)
	      cout<<"<span style=\""<<style<<"\">"<<positions[column]<<"</span>";
	    else
	      cout<<"<span>"<<positions[column]<<"</span>";
	  }
	  cout<<"</tr>\n";
	}

	for(int i=0;i<T.n_leaves();i++) {
	  int s = i;
	  cout<<"  <tr>"<<endl;
	  cout<<"    <td class=\"sequencename\">"<<T.seq(s)<<"</td>"<<endl;
	  cout<<"    <td>";
	  for(int column=pos;column<pos+width and column < end; column++) {
	    string c;
	    c+= a.lookup(A(column,s));
	    if (A.gap(column,s)) {
	      if (not showgaps)
		c = "&nbsp;";
	    }
	    string style = getstyle(colors(column,s),c,color_scheme);
	    cout<<"<span style=\""<<style<<"\">"<<c<<"</span>";
	  }
	  cout<<"    </td>";
	  cout<<"  </tr>"<<endl;
	}
	cout<<"<tr><td></td><td><span>&nbsp;</span></td><tr>\n";
	cout<<"</table>"<<endl;
	pos += width;
      }
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
