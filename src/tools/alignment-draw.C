#include <cmath>
#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "colors.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using namespace colors;

// some scale functions
double identity(double x) {return x;}
double square(double x) {return x*x;}
double cube(double x) {return x*x*x;}

double LOD10(double x) {return log10(x) - log10(1.0-x);}

/// A representation of a transformation of [0,1] (certainty) onto [0,1] (colormap scale)
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


/// ColorMap base class - gives fg and bg colors for string 's' based on x in [0,1]
class ColorMap {
public:
  virtual ColorMap* clone() const=0;

  virtual RGB bg_color(double x,const string& s) const=0;
  virtual RGB fg_color(double x,const string& s) const=0;

  virtual ~ColorMap() {}
};

/// ColorMap which gives black on white
struct Plain_ColorMap: public ColorMap {
public:
  Plain_ColorMap* clone() const {return new Plain_ColorMap(*this);}

  RGB bg_color(double,const string&) const { return white; }

  RGB fg_color(double,const string&) const { return black; }
};

/// ColorMap which represents uncertainty in terms of gray-scale
struct BW_ColorMap: public ColorMap {
public:
  BW_ColorMap* clone() const {return new BW_ColorMap(*this);}

  RGB bg_color(double x,const string&) const {
    return HSV(0,0,1.0-x);
  }

  RGB fg_color(double x,const string&) const {
    if (x < 0.5)
      return black;
    else
      return white;
  }
};

/// ColorMap which represents uncertainty in terms of Rainbow colors
struct Rainbow_ColorMap: public ColorMap {
  double start;
  double end;

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
      double hue     = start + x*(end - start);

      return HSV(hue,1.0,0.95).to_RGB();
    }
  }

  RGB fg_color(double,const string&) const {
    return black;
  }
 
 Rainbow_ColorMap() :start(0.666),end(0)
  { }

  Rainbow_ColorMap(double h1,double h2)
    :start(h1),end(h2)
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
  else if (strchr("ACDENQZB",aa))
    return yellow;
  else if (strchr("-*+X ",aa))
    return grey;
  
  throw myexception()<<"Letter '"<<aa<<" does not appear to be an amino acid";
}


/// ColorMap which uses the bg color to display AA type
class AA_colors: public ColorMap 
{
public:

  AA_colors* clone() const {return new AA_colors(*this);}

  RGB bg_color(double,const string& s) const {
    if (s.length() > 1) std::abort();
    char aa = ' ';
    if (not s.empty())
      aa = s[0];
    
    return AA_color(aa);
  }

  RGB fg_color(double,const string&) const {
    return black;
  }
};

RGB DNA_color(char aa) {
  if (strchr("A",aa))
    return red; 
  else if (strchr("TU",aa))
    return green;
  else if (strchr("G",aa))
    return orange;
  else if (strchr("C",aa))
    return blue;
  else if (strchr("-*+NYR ",aa))
    return grey;
  
  throw myexception()<<"Letter '"<<aa<<" does not appear to be an nucleic acid";
}


/// ColorMap which uses the bg color to display AA type
class DNA_colors: public ColorMap 
{
public:

  DNA_colors* clone() const {return new DNA_colors(*this);}

  RGB bg_color(double,const string& s) const {
    if (s.length() > 1) std::abort();
    char aa = ' ';
    if (not s.empty())
      aa = s[0];
    
    return DNA_color(aa);
  }

  RGB fg_color(double,const string&) const {
    return black;
  }
};


/// ColorMap which makes the bg color fade almost to white if uncertain
class whiten_colors: public ColorMap {
  OwnedPointer<ColorMap> sub_map;
public:
  whiten_colors* clone() const {return new whiten_colors(*this);}

  RGB bg_color(double x,const string& s) const {
    double x2 = x;
    if (x2<0.30) x2 = 0.30;
    return whiten(sub_map->bg_color(x,s),1.0-sqrt(x2));
  }

  RGB fg_color(double x,const string& s) const {
    if (x<0.2) x = 0.2;
    return whiten(sub_map->fg_color(x,s),1.0-sqrt(sqrt(x)));
  }

  whiten_colors(const ColorMap& colors)
    :sub_map(colors) 
  {}
};

/// ColorMap which switches the foreground and background colors
class switch_fg_bg: public ColorMap {
  OwnedPointer<ColorMap> sub_map;
public:
  switch_fg_bg* clone() const {return new switch_fg_bg(*this);}

  RGB bg_color(double x,const string& s) const {
    return sub_map->fg_color(x,s);
  }

  RGB fg_color(double x,const string& s) const {
    return sub_map->bg_color(x,s);
  }

  switch_fg_bg(const ColorMap& colors)
    :sub_map(colors) 
  {}
};


/// ColorMap which sets the foreground color for best contrast
class contrast: public ColorMap {
  OwnedPointer<ColorMap> sub_map;
public:
  contrast* clone() const {return new contrast(*this);}

  RGB bg_color(double x,const string& s) const {
    return sub_map->bg_color(x,s);
  }

  RGB fg_color(double x,const string& s) const {
    double whiteness = grayscale(sub_map->bg_color(x,s));
    if (whiteness < 0.55)
      return white;
    else
      return black;
  }

  contrast(const ColorMap& colors)
    :sub_map(colors) 
  {}
};

class ColorScheme: public Cloneable {
  Scale scale;
  OwnedPointer<ColorMap> color_map;
public:
  virtual ColorScheme* clone() const {return new ColorScheme(*this);}

  RGB bg_color(double x,const string& s) const {
    return color_map->bg_color(scale(x),s);
  }

  RGB fg_color(double x,const string& s) const {
    return color_map->fg_color(scale(x),s);
  }

  ColorScheme(const ColorMap& CM,const Scale& S)
    :scale(S),color_map(CM)
  { }
};

using std::cout;
using std::cerr;
using std::endl;

string getstyle(double d,const string& s,const ColorScheme& color_scheme) {
  string style;
  if ((s == "?") or (s == "???")) {
    style += "background: white;";
    style += "color: white;";
  }
  else {
    style += "background: " + color_scheme.bg_color(d,s).to_css() + ";" ;
    style += "color: "      + color_scheme.fg_color(d,s).to_css() + ";" ;
  }
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


ublas::matrix<double> read_alignment_certainty(const alignment& A, const string& filename) 
{
  ifstream colorfile(filename.c_str());

  vector<int> mapping;
  {
    string line;
    getline(colorfile,line);
    vector<string> colornames = split(line,' ');
    vector<string> leafnames;
    for(int i=0;i<A.n_sequences();i++)
      leafnames.push_back(A.seq(i).name);
    mapping = compute_mapping(colornames,leafnames);
  }

  // Add an entry for the ENTIRE COLUMN
  mapping.push_back(mapping.size());

  //------------------ Read in the colors ------------------------//
  ublas::matrix<double> colors(A.length(),A.n_sequences()+1);
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
  o<<"uncertain ";
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


Scale get_scale(const variables_map& args) {
  Scale scale;
  string scale_name = args["scale"].as<string>();
  if (scale_name == "identity") {
    scale.f = identity;
    scale.min = 0;
    scale.max = 1;
    if (args.count("min")) scale.min = args["min"].as<double>();
    if (args.count("max")) scale.max = args["max"].as<double>();
  }
  else if (scale_name == "square") {
    scale.f = square;
    scale.min = 0;
    scale.max = 1;
    if (args.count("min")) scale.min = args["min"].as<double>();
    if (args.count("max")) scale.max = args["max"].as<double>();
  }
  else if (scale_name == "cube") {
    scale.f = cube;
    scale.min = 0;
    scale.max = 1;
    if (args.count("min")) scale.min = args["min"].as<double>();
    if (args.count("max")) scale.max = args["max"].as<double>();
  }
  else if (scale_name == "LOD") {
    scale.f = LOD10;
    scale.min = -0.5;
    scale.max = 2.0;
    if (args.count("min")) scale.min = args["min"].as<double>();
    if (args.count("max")) scale.max = args["max"].as<double>();
  }
  else
    throw myexception()<<"Unrecognized scale '"<<scale_name<<"'.";
  return scale;
}

OwnedPointer<ColorMap> get_base_color_map(vector<string>& string_stack)
{
  OwnedPointer<ColorMap> color_map;
  if (match(string_stack,"plain"))
    color_map = OwnedPointer<ColorMap>(new Plain_ColorMap);
  else if (match(string_stack,"bw"))
    color_map = OwnedPointer<ColorMap>(new BW_ColorMap);
  else if (match(string_stack,"RedBlue"))
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap(0.7,1));
  else if (match(string_stack,"BlueRed"))
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap(1,0.7));
  else if (match(string_stack,"Rainbow"))
    color_map = OwnedPointer<ColorMap>(new Rainbow_ColorMap);
  else if (match(string_stack,"AA"))
    color_map = OwnedPointer<ColorMap>(new AA_colors);
  else if (match(string_stack,"DNA"))
    color_map = OwnedPointer<ColorMap>(new DNA_colors);
  else if (match(string_stack,"RNA"))
    color_map = OwnedPointer<ColorMap>(new DNA_colors);
  else
    throw myexception()<<"Unrecognized base color scheme '"<<string_stack.back()<<"'";

  return color_map;
}


OwnedPointer<ColorMap> get_color_map(const variables_map& args) 
{
  // get the string stack
  vector<string> string_stack;
  if (args.count("color-scheme"))
    string_stack = split(args["color-scheme"].as<string>(),'+');
  else if (args.count("AU"))
    string_stack = split("Rainbow+contrast+fade",'+');
  else
    string_stack = split("plain",'+');

  std::reverse(string_stack.begin(),string_stack.end());
  if (string_stack.empty())
    throw myexception()<<"Color scheme not specified. (but how?)";
  
  OwnedPointer<ColorMap> color_map = get_base_color_map(string_stack);;

  while(string_stack.size()) {
    if (match(string_stack,"switch"))
      color_map = switch_fg_bg(*color_map);
    else if (match(string_stack,"contrast"))
      color_map = contrast(*color_map);
    else if (match(string_stack,"fade"))
      color_map = whiten_colors(*color_map);
    else
      throw myexception()<<"Unrecognized color scheme modifier '"<<string_stack.back()<<"'";
  }
  return color_map;
}

OwnedPointer<ColorScheme> get_color_scheme(const variables_map& args) 
{
  OwnedPointer<ColorMap> color_map = get_color_map(args);
  assert(color_map);

  Scale scale = get_scale(args);

  return ColorScheme(*color_map,scale);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description general("General options");
  general.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ;
  
  options_description output("Output");
  output.add_options()
    ("no-legend","don't print a legend") 
    ("show-ruler","print a ruler to show column numbers") 
    ("column-colors","color-code column ticks by column certainty") 
    ("AU",value<string>(),"file with alignment uncertainties")
    ("show-gaps",value<string>()->default_value("yes"),"show gaps") 
    ("width",value<int>()->default_value(70),"the number of columns per line")
    ("start",value<int>(),"the first column to plot")
    ("end",value<int>(),"the last column to plot")
    ("format",value<string>()->default_value("HTML"),"produce a plot in this format")
    ("min",value<double>(),"minimum value of scale function")
    ("max",value<double>(),"maximum value of scale function")
    ("color-scheme",value<string>(),"include a length of how certainties map to colors")
    ("scale",value<string>()->default_value("LOD"),"scale for the uncertainties")
    ;

  options_description all("All options");
  all.add(general).add(output);

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("AU", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);

  if (args.count("help")) {
    cout<<"Usage: alignment-draw <alignment> [<AU file>] [OPTIONS]\n";
    cout<<all<<"\n";
    cout<<"Base Color Schemes:\n";
    cout<<"    plain, bw, Rainbow, RedBlue, BlueRed, AA, DNA\n";
    cout<<"\n";
    cout<<"Modifiers:\n";
    cout<<"    switch, contrast, fade\n";
    cout<<"\n";
    cout<<"Examples:\n";
    cout<<"    Rainbow+contrast+fade\n";
    cout<<"    AA+contrast+fade+fade+fade+fade\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 

  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    bool columncolors = args.count("column-colors")==1;

    bool show_column_numbers = args.count("show-ruler")==1;

    bool showgaps = true;
    if (args.count("show-gaps") and args["show-gaps"].as<string>() == "no")
      showgaps = false;

    //---------- Load alignment and tree -----------//
    alignment A = load_A(args,false);

    //---- Find mapping from colorfile to alignment sequence order -----//
    OwnedPointer<ColorScheme> color_scheme = get_color_scheme(args);

    //---------- Read alignment uncertainty, if available --------------//
    ublas::matrix<double> colors(A.length(),A.n_sequences()+1);
    
    if (args.count("AU"))
      colors = read_alignment_certainty(A,args["AU"].as<string>());
    else
      for(int i=0;i<colors.size1();i++)
	for(int j=0;j<colors.size2();j++)
	  colors(i,j) = 0;

    //-------------------- Get width -----------------------//
    int width = args["width"].as<int>();

    //-------------------- Get start -----------------------//
    int start=0;
    if (args.count("start")) {
      start = args["start"].as<int>();
      if (start < 0)
	throw myexception()<<"Parameter 'start' must be positive.";
      if (not (start < A.length()))
	throw myexception()<<"Parameter 'start' must be less than the length of the alignment ("<<A.length()<<").";
    }
    
    //--------------------- Get end ------------------------//
    int end = A.length()-1;
    if (args.count("end")) {
      end =args["end"].as<int>();
      if (end < 0)
	throw myexception()<<"Parameter 'end' must be positive.";
      if (not (end < A.length()))
	throw myexception()<<"Parameter 'end' must be less than the length of the alignment ("<<A.length()<<").";
      if (end < start)
	throw myexception()<<"Parameter 'end' must be >= than parameter 'start'"<<A.length()<<").";
    }

    if (args["format"].as<string>() == "latex") {
      std::cout<<"\\documentclass[10pt]{article}\n\
\\usepackage{colortbl}\n\
\\begin{document}\n\
\\definecolor{c2}{rgb}{1.0,0.5,0.0}\n";

      /*-------------------- Print the alignment ------------------------*/
      const alphabet& a = A.get_alphabet();
    
      int pos=start;
      while(pos<=end) {
	cout<<"\\begin{tabular}{";
	for(int i=0;i<width;i++)
	  cout<<"c";
	cout<<"}\n";
	
	for(int i=0;i<A.n_sequences();i++) {
	  int s = i;

	  for(int column=pos;column<pos+width and column <= end; column++) {
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
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
 <head>\n\
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />\n\
  <title>Alignment Uncertainty: "<<args["align"].as<string>()<<"</title>\n\
  <style type=\"text/css\">\n\
\n\
TABLE {\n\
   border: none;\n\
   border-collapse: collapse;\n\
   border-spacing: 0;\n\
   margin-top: 0.4em;\n\
}\n\
\n\
BODY {\n\
  font-size: 12pt;\n\
  font-family: helvetica;\n\
}\n\
\n\
.sequences TD { \n\
   padding: 0.1em;\n\
   margin: 0;\n\
\n\
   font-size: 10pt; \n\
   font-weight: bold;\n\
   font-family: monospace;\n\
}\n\
\n\
.sequences TD.sequencename {\n\
   padding-right: 1em;\n\
\n\
   font-weight: normal;\n\
   font-style: italic;\n\
   font-family: sans-serif;\n\
}\n\
\n\
.legend {\n\
//  width: 100%;\n\
//  text-align: center;\n\
}\n\
    </style>\n\
  </head>\n\
  <body>\n\n";

      //-------------------- Print a legend ------------------------//
      if (not args.count("no-legend")) {
	cout<<"\n<table class=\"legend\"><tr>\n";

	cout<<"<td>";
	draw_legend(cout,*color_scheme,"");
	cout<<"</td>\n";

	cout<<"<td>";
	draw_legend(cout,*color_scheme,"-");
	cout<<"</td>\n";

	cout<<"</tr></table>\n";
      }

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
      while(pos<=end) {
	cout<<"\n\n<table class=\"sequences\">\n";

	// Print columns positions
	if (show_column_numbers) {
	  cout<<"<tr><td></td>";
	  
	  for(int column=pos;column<pos+width and column <= end; column++) {
	    double P=colors(column,A.n_sequences());
	    string style = getstyle(P,"",*color_scheme);
	    if (columncolors)
	      cout<<"<td style=\""<<style<<"\">"<<positions[column]<<"</td>";
	    else
	      cout<<"<td>"<<positions[column]<<"</td>";
	  }
	  cout<<"</tr>\n";
	}

	for(int i=0;i<A.n_sequences();i++) {
	  int s = i;
	  cout<<"  <tr>\n";
	  cout<<"    <td class=\"sequencename\">"<<A.seq(s).name<<"</td>\n";
	  for(int column=pos;column<pos+width and column <= end; column++) {
	    string c;
	    c+= a.lookup(A(column,s));
	    if (A.gap(column,s)) {
	      if (not showgaps)
		c = "&nbsp;";
	    }
	    string style = getstyle(colors(column,s),c,*color_scheme);
	    cout<<"<td style=\""<<style<<"\">"<<c<<"</td>";
	  }
	  cout<<"  </tr>\n";
	}
	cout<<"<tr><td></td><td>&nbsp;</td></tr>\n";
	cout<<"</table>"<<endl;
	pos += width;
      }
      cout<<"</body>\n</html>\n";
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

