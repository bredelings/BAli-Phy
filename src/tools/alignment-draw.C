#include <cmath>
#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "util.H"
#include "setup.H"
#include "colors.H"

using namespace colors;

double identity(double x) {return x;}
double square(double x) {return x*x;}
double cube(double x) {return x*x*x;}

double LOD10(double x) {return log10(x) - log10(1.0-x);}

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

  virtual RGB bg_color(double x,const string& s) const=0;
  virtual RGB fg_color(double x,const string& s) const=0;
};

struct Plain_ColorMap: public ColorMap {
public:
  Plain_ColorMap* clone() const {return new Plain_ColorMap(*this);}

  RGB bg_color(double x,const string& s) const { return white; }

  RGB fg_color(double x,const string& s) const { return black; }
};

struct BW_ColorMap: public ColorMap {
public:
  BW_ColorMap* clone() const {return new BW_ColorMap(*this);}

  RGB bg_color(double x,const string& s) const {
    return HSV(0,0,1.0-x);
  }

  RGB fg_color(double x,const string& s) const {
    if (x < 0.5)
      return black;
    else
      return white;
  }
};

struct Rainbow_ColorMap: public ColorMap {
  double h_start;
  double h_end;

  double s_start;
  double s_end;

public:
  Rainbow_ColorMap* clone() const {return new Rainbow_ColorMap(*this);}

  RGB bg_color(double x,const string& s) const {
    if ((s == "-") or (s == "---")) {
      double v_uncertain = 1.0;
      double v_certain   = 0.6;
      double value = v_uncertain + x*(v_certain - v_uncertain);

      return HSV(0,0,value).to_RGB();
    }
    else {
      double hue     = h_start + x*(h_end - h_start);

      double saturation = s_start + x*(s_end - s_start);

      return HSV(hue,saturation,0.95).to_RGB();
    }
  }

  RGB fg_color(double x,const string& s) const {
    if (x < 0.5)
      return black;
    else
      return white;
      
  }

  Rainbow_ColorMap() :h_start(0.75),h_end(0),s_start(0.3),s_end(0.95)
  { }

  Rainbow_ColorMap(double h1,double h2)
    :h_start(h1),h_end(h2),s_start(0.3),s_end(0.95)
  { }

  Rainbow_ColorMap(double h1,double h2,double s1,double s2)
    :h_start(h1),h_end(h2),s_start(s1),s_end(s2)
  { }
};

class ColorScheme: public Cloneable {
public:
  virtual ColorScheme* clone() const =0;

  virtual RGB bg_color(double x,const string& s) const =0;

  virtual RGB fg_color(double x,const string& s) const =0;

  virtual ~ColorScheme() {}
};

// FIXME: all the nested stuff should really be color MAPs
// There should be only one scale, and it should be at the top level
// That way, all the colormaps will know how they correspond.  
//   For example, the fade colormap would be able to set the whitening to
//   correspond to the transition from red to yellow...

// FIXME: make a separate 'scale' argument.

// Hmm... Marc's fg-contrast fits naturally into this scheme.

// FIXME: add a grayscale function to colors.C.

// FIXME: merge rgb and hsv into a single class? Then only R() or H() could
//        be a reference, not both...


// color-scheme=Rainbow
// color-scheme=plain
// color-scheme=bw
// color-scheme=bw-ify  (get colors, turn it into a greyscale value)

// color-scheme=plain+bg=AminoAcid+bg=fade
// color-scheme=Rainbow+fg=AminoAcid
// color-scheme=bw+switch+fg=AminoAcid

// Hmm...  color-scheme=plain+AA+fade ?
///                    =plain-AA      affect fg instead?

class BaseColorScheme: public ColorScheme {
  Scale scale;
  OwnedPointer<ColorMap> color_map;
public:
  virtual BaseColorScheme* clone() const {return new BaseColorScheme(*this);}

  RGB bg_color(double x,const string& s) const {
    return color_map->bg_color(scale(x),s);
  }

  RGB fg_color(double x,const string& s) const {
    return color_map->fg_color(scale(x),s);
  }

  BaseColorScheme(const ColorMap& CM,const Scale& S)
    :scale(S),color_map(CM)
  { }
};

RGB AA_color(char aa) {
  if (strchr("GPST",aa))
    return orange;
  else if (strchr("HKR",aa))
    return red;
  else if (strchr("FWY",aa))
    return blue;
  else if (strchr("ILMV",aa))
    return green;
  else if (strchr("ACDENQ",aa))
    return yellow;
  else if (strchr("-*+X ",aa))
    return grey;
  
  throw myexception()<<"Letter '"<<aa<<" does not appear to be an amino acid";
}


class AA_fg_colors: public ColorScheme {
  OwnedPointer<ColorScheme> sub_scheme;
public:

  AA_fg_colors* clone() const {return new AA_fg_colors(*this);}

  RGB bg_color(double x,const string& s) const {
    return sub_scheme->bg_color(x,s);
  }

  RGB fg_color(double x,const string& s) const {
    if (s.length() > 1) std::abort();
    char aa = ' ';
    if (not s.empty())
      aa = s[0];
    
    return AA_color(aa);
  }

  AA_fg_colors(const ColorScheme& scheme)
    :sub_scheme(scheme) 
  {}
};

class AA_bg_colors: public ColorScheme {
  OwnedPointer<ColorScheme> sub_scheme;
public:

  AA_bg_colors* clone() const {return new AA_bg_colors(*this);}

  RGB bg_color(double x,const string& s) const {
    if (s.length() > 1) std::abort();
    char aa = ' ';
    if (not s.empty())
      aa = s[0];
    
    return AA_color(aa);
  }

  RGB fg_color(double x,const string& s) const {
    return sub_scheme->fg_color(x,s);
  }

  AA_bg_colors(const ColorScheme& scheme)
    :sub_scheme(scheme) 
  {}
};

class switch_fg_bg: public ColorScheme {
  OwnedPointer<ColorScheme> sub_scheme;
public:
  switch_fg_bg* clone() const {return new switch_fg_bg(*this);}

  RGB bg_color(double x,const string& s) const {
    return sub_scheme->fg_color(x,s);
  }

  RGB fg_color(double x,const string& s) const {
    return sub_scheme->bg_color(x,s);
  }

  switch_fg_bg(const ColorScheme& scheme)
    :sub_scheme(scheme) 
  {}
};

class bg_fade_uncertainty: public ColorScheme {
  OwnedPointer<ColorScheme> sub_scheme;
public:
  bg_fade_uncertainty* clone() const {return new bg_fade_uncertainty(*this);}

  RGB bg_color(double x,const string& s) const {
    if (x<0.35) x = 0.35;
    return whiten(sub_scheme->bg_color(x,s),1.0-x*x*x);
  }

  RGB fg_color(double x,const string& s) const {
    if (x<0.25) x = 0.25;
    return whiten(sub_scheme->fg_color(x,s),1.0-x);
  }

  bg_fade_uncertainty(const ColorScheme& scheme)
    :sub_scheme(scheme) 
  {}
};

using std::cout;
using std::cerr;
using std::endl;

string getstyle(double d,const string& s,const ColorScheme& color_scheme) {
  string style;
  style += "background: " + color_scheme.bg_color(d,s).to_css() + ";" ;
  style += "color: "      + color_scheme.fg_color(d,s).to_css() + ";" ;
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

void draw_legend(std::ostream& o,ColorScheme& color_scheme,const string& letter) {
  o<<"<P>uncertain ";
  const int nsquares = 40;
  for(int i = 0;i < nsquares;i++) {
    double p = (0.5+i)/nsquares;
    string style = getstyle(p,letter,color_scheme);
    o<<"<span style=\""<<style<<"\">&nbsp;</span>";
  }
  o<<" certain";
}

/// Take something off the string stack, if its present
bool match(vector<string>& sstack,const string& s) {
  bool m = false;
  if (sstack.size() and sstack.back() == s) {
    m = true;
    sstack.pop_back();
  }
  return m;
}

OwnedPointer<ColorScheme> get_base_color_scheme(Arguments& args,
						const string& color_map_name,
						const string& scale_name) 
{
  OwnedPointer<ColorMap> color_map;
  if (color_map_name == "plain")
    color_map = OwnedPointer<ColorMap>(new Plain_ColorMap);    
  else if (color_map_name == "bw")
    color_map = OwnedPointer<ColorMap>(new BW_ColorMap);    
  else if (color_map_name == "RedBlue") 
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap(0.7,1,0.95,0.95));
  else if (color_map_name == "BlueRed") 
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap(1,0.7,0.95,0.95));
  else if (color_map_name == "Rainbow")
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap);
  else
    return OwnedPointer<ColorScheme>(NULL);

  Scale scale;
  if (scale_name == "identity") {
    scale.f = identity;
    scale.min = args.loadvalue("min",0.0);
    scale.max = args.loadvalue("max",1.0);
  }
  else if (scale_name == "square") {
    scale.f = square;
    scale.min = args.loadvalue("min",0.0);
    scale.max = args.loadvalue("max",1.0);
  }
  else if (scale_name == "cube") {
    scale.f = cube;
    scale.min = args.loadvalue("min",0.0);
    scale.max = args.loadvalue("max",1.0);
  }
  else if (scale_name == "LOD") {
    scale.f = LOD10;
    scale.min = args.loadvalue("min",-0.5);
    scale.max = args.loadvalue("max",2.0);
  }
  else
    return OwnedPointer<ColorScheme>(NULL);

  return BaseColorScheme(*color_map,scale);
}


OwnedPointer<ColorScheme> get_base_color_scheme(Arguments& args, vector<string>& string_stack) 
{
  OwnedPointer<ColorScheme> result;

  if (string_stack.size()) {
    vector<string> model = split(string_stack.back(),'*');
    if (model.size() == 2)
      result = get_base_color_scheme(args,model[0],model[1]);
    else if (model.size() == 1) {
      result = get_base_color_scheme(args,model[0],"LOD");
      if (not result) result = get_base_color_scheme(args,"Rainbow",model[0]);
    }
    else 
      throw myexception()<<"Can't parse base color scheme '"<<string_stack.back()<<"'";
  }

  if (result)
    string_stack.pop_back();
  else
    result = get_base_color_scheme(args,"Rainbow","LOD");

  return result;
}

OwnedPointer<ColorScheme> get_color_scheme(Arguments& args) 
{
  vector<string> string_stack;
  if (args["color-scheme"] != "")
    string_stack = split(args["color-scheme"],'+');
  std::reverse(string_stack.begin(),string_stack.end());
  
  OwnedPointer<ColorScheme> color_scheme = get_base_color_scheme(args,string_stack);
  assert(color_scheme);

  while(string_stack.size()) {
    if (match(string_stack,"switch"))
      color_scheme = switch_fg_bg(*color_scheme);
    else if (match(string_stack,"fg=AminoAcid"))
      color_scheme = AA_fg_colors(*color_scheme);
    else if (match(string_stack,"bg=AminoAcid"))
      color_scheme = AA_bg_colors(*color_scheme);
    else if (match(string_stack,"bg=fade"))
      color_scheme = bg_fade_uncertainty(*color_scheme);
    else
      throw myexception()<<"Can't parse color scheme modifier '"<<string_stack.back()<<"'";
  }
  return color_scheme;
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
    if (not args.set("AU-file"))
      throw myexception()<<"AU probabilites file not specified! (AU-file=<filename>)";

    ublas::matrix<double> colors = read_alignment_certainty(A,T,args["AU-file"].c_str());

    OwnedPointer<ColorScheme> color_scheme = get_color_scheme(args);

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
	cout<<"}\n";
	
	for(int i=0;i<T.n_leaves();i++) {
	  int s = i;

	  for(int column=pos;column<pos+width and column < end; column++) {
	    string c = a.lookup(A(column,s));
	    string latexcolor = "";//latex_get_bgcolor(colors(column,s),sscale,color);
	    if (column != pos)
	      cout<<"& ";
	    cout<<"\\multicolumn{1}{>{\\columncolor{"<<latexcolor<<"}}c}{"<<c<<"}"<<"\n";
	  }
	  cout<<"\\\\\n";
	}
	cout<<"\\end{tabular}\n";
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
	draw_legend(cout,*color_scheme,"");
	draw_legend(cout,*color_scheme,"-");
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
	cout<<"<table>\n";

	// Print columns positions
	if (show_column_numbers) {
	  cout<<"<tr><td></td><td>";
	  
	  for(int column=pos;column<pos+width and column < end; column++) {
	    double P=colors(column,T.n_leaves());
	    string style = getstyle(P,"",*color_scheme);
	    if (columncolors)
	      cout<<"<span style=\""<<style<<"\">"<<positions[column]<<"</span>";
	    else
	      cout<<"<span>"<<positions[column]<<"</span>";
	  }
	  cout<<"</tr>\n";
	}

	for(int i=0;i<T.n_leaves();i++) {
	  int s = i;
	  cout<<"  <tr>\n";
	  cout<<"    <td class=\"sequencename\">"<<T.seq(s)<<"</td>\n";
	  cout<<"    <td>";
	  for(int column=pos;column<pos+width and column < end; column++) {
	    string c;
	    c+= a.lookup(A(column,s));
	    if (A.gap(column,s)) {
	      if (not showgaps)
		c = "&nbsp;";
	    }
	    string style = getstyle(colors(column,s),c,*color_scheme);
	    cout<<"<span style=\""<<style<<"\">"<<c<<"</span>";
	  }
	  cout<<"    </td>";
	  cout<<"  </tr>\n";
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


//FIXME - IMPLEMENT
// EPS output - use libplot - unmaintained for 4 years?
// make a STACK of things which modify the previous one - e.g. whiten the color
//    - We still need a bottom layer
// can we make the cutoff (e.g. min) different for hue and saturation?
//    - It seems that hsv is a good model - if plugins modify it, then different 
//      plugins could have different mins. (But it should perhaps be independant of 
//      the "scale" used.)

// plugins:
//   : color-gaps-differently.
//   : whiten-below-0.5
//   : fore-ground-colors (AA)
//   : whiten-according to uncertainty

