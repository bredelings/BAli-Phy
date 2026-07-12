/*
  Copyright (C) 2004-2009 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

#include <cmath>
#include <iostream>
#include <fstream>
#include <map>
#include <optional>
#include <sstream>
#include <string_view>
#include "tree/tree.H"
#include "sequence/alphabet.H"
#include "sequence/sequence.H"
#include "sequence/sequence-format.H"
#include "util/matrix.H"
#include "util/json.hh"
#include "util/string/split.H"
#include "util/mapping.H"
#include "util/io.H"
#include "util/string/convert.H"
#include "colors.H"
#include "alignment-draw-assets.hh"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::vector;
using std::ifstream;
using namespace colors;

// some scale functions
double identity(double x) {return x;}
double invert(double x) {return 1.0-x;}
double square(double x) {return x*x;}
double cube(double x) {return x*x*x;}

double LOD10(double x) {return log10(x) - log10(1.0-x);}

/// A representation of a transformation of [0,1] (certainty) onto [0,1] (colormap scale)
struct Scale {
    double min;
    double max;
    double (*f)(double);

    double operator()(double p) const 
	{
	    // pass-through
	    if (p < 0) return -1;

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
struct Rainbow_ColorMap: public ColorMap 
{
    double start;
    double end;
    bool gaps_different;

public:

    Rainbow_ColorMap* clone() const {return new Rainbow_ColorMap(*this);}

    RGB bg_color(double x,const string& s) const 
	{
	    // pass-through
	    if (x < 0) return white;

	    // color gaps differently? (grey-scale?)
	    if (gaps_different and ((s == "-") or (s == "---"))) {
		double v_uncertain = 1.0;
		double v_certain   = 0.6;
		double value = v_uncertain + x*(v_certain - v_uncertain);

		return HSV(0,0,value).to_RGB();
	    }
	    // otherwise use the rainbow
	    else {
		double hue     = start + x*(end - start);

		return HSV(hue,1.0,0.95).to_RGB();
	    }
	}

    RGB fg_color(double,const string&) const {
	return black;
    }
 
    Rainbow_ColorMap(bool g=true) 
	:start(0.666),end(0),gaps_different(g)
	{ }

    Rainbow_ColorMap(double h1,double h2,bool g=true)
	:start(h1),end(h2),gaps_different(g)
	{ }
};

/// ColorMap which represents uncertainty in terms of Diff colors
struct Diff_ColorMap: public ColorMap 
{
    int n_blocks;
    double start;
    double end;
    bool gaps_different;
    
public:

    Diff_ColorMap* clone() const {return new Diff_ColorMap(*this);}

    RGB bg_color(double x,const string& s) const 
	{
	    // color gaps differently? (grey-scale?)
	    if (gaps_different and ((s == "-") or (s == "---")))
	    {
		double v_uncertain = 1.0;
		double v_certain   = 0.6;
		double value = v_uncertain + 0*(v_certain - v_uncertain);

		return HSV(0,0,value).to_RGB();
	    }
	    else
	    {
		if (x == 0) return white;

		if (n_blocks == 1) return HSV(0,1,1);
		
		x = (x-1)/(n_blocks-1);

		// otherwise use the rainbow
		double hue     = start + x*(end - start);
		double sat     = 1.0 - x*0.5;

		return HSV(hue,sat,0.95).to_RGB();
	    }
	}

    RGB fg_color(double,const string&) const {
	return black;
    }
 
    Diff_ColorMap(int i,bool g=true)
	:n_blocks(i),start(0.15),end(0.55),gaps_different(g)
	{ }

    Diff_ColorMap(int i,double s,double e,bool g=true)
	:n_blocks(i),start(s),end(e),gaps_different(g)
	{ }
};

RGB AA_color_old(char aa) {

    if (strchr("GPST",aa))
	return orange;
    else if (strchr("HKR",aa))
	return red;
    else if (strchr("FWY",aa))
	return blue;
    else if (strchr("ILMVJ",aa))
	return green;
    else if (strchr("ACDENQZB",aa))
	return yellow;
    else if (strchr("-*+X ",aa))
	return grey;
  
    throw myexception()<<"Letter '"<<aa<<"' does not appear to be an amino acid";
}


RGB AA_color(char aa)
{
    if (aa == 'G')
	return rgb(0xff, 0xb1, 0x59);
    else if (aa == 'P')
	return rgb(0xec, 0xec, 0x00);
    else if (aa == 'S')
	return rgb(0xad, 0x02, 0xad);
    else if (aa == 'T')
	return rgb(0x50, 0xe8, 0x50);
    else if (aa == 'H')
	return rgb(0x1d, 0xcf, 0xcf);
    else if (aa == 'K')
	return rgb(0xff, 0x3b, 0x1d);
    else if (aa == 'R')
	return rgb(0xff, 0x4f, 0x33);
    else if (aa == 'F')
	return rgb(0x1d, 0x94, 0xff);
    else if (aa == 'W')
	return rgb(0x04, 0x62, 0xc3);
    else if (aa == 'Y')
	return rgb(0x17, 0xe5, 0xe7);
    else if (aa == 'I')
	return rgb(0x5a, 0xb9, 0xff);
    else if (aa == 'L')
	return rgb(0x8b, 0xc0, 0xf6);
    else if (aa == 'M')
	return rgb(0x11, 0x61, 0xb3);
    else if (aa == 'V')
	return rgb(0x05, 0x90, 0xff);
    else if (aa == 'A')
	return rgb(0x2d, 0x7f, 0xd3);
    else if (aa == 'C')
	return rgb(0xff, 0x94, 0x94);
    else if (aa == 'D')
	return rgb(0xed, 0x61, 0xed);
    else if (aa == 'E')
	return rgb(0xaf, 0x4a, 0xae);
    else if (aa == 'N')
	return rgb(0x1d, 0xec, 0x1d);
    else if (aa == 'Q')
	return rgb(0x6a, 0xff, 0x6a);
    else if (aa == 'Z') // E or Q
	return rgb((0xaf + 0x6a)/2, (0x4a + 0xff)/2 , (0xae + 0x6a)/2);
    else if (aa == 'B') // D or N
	return rgb((0xed + 0x1d)/2, (0x61+0xec)/2, (0xed+0x1d)/2);
    else if (aa == 'J') // L or I
	return rgb((0x5a+0x8a)/2, (0xb9+0xc0)/2, (0xff+0xf6)/2);
    else if (strchr("-*+X ",aa))
	return grey;

    throw myexception()<<"Letter '"<<aa<<"' does not appear to be an amino acid";
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
	return yellow;
    else if (strchr("C",aa))
	return blue;
    else if (strchr("-*+NYRWSMKBDHV ",aa))
	return grey;
  
    throw myexception()<<"Letter '"<<aa<<"' does not appear to be an nucleic acid";
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
    owned_ptr<ColorMap> sub_map;
    double min_fg;
    double min_bg;
public:
    whiten_colors* clone() const {return new whiten_colors(*this);}

    RGB bg_color(double x,const string& s) const {
	double x2 = std::max(x,min_bg);
	return whiten(sub_map->bg_color(x,s),1.0-sqrt(x2));
    }

    RGB fg_color(double x,const string& s) const {
	x = std::max(x,min_fg);
	return whiten(sub_map->fg_color(x,s),1.0-sqrt(sqrt(x)));
    }

    whiten_colors(const ColorMap& colors,double m_fg=0.2,double m_bg=0.3)
	:sub_map(colors),min_fg(m_fg), min_bg(m_bg)
	{}
};

/// ColorMap which switches the foreground and background colors
class switch_fg_bg: public ColorMap {
    owned_ptr<ColorMap> sub_map;
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
    owned_ptr<ColorMap> sub_map;
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

class ColorScheme
{
    std::function<double(double)> transform;
    owned_ptr<ColorMap> color_map;
public:
    ColorScheme* clone() const {return new ColorScheme(*this);}

    RGB bg_color(double x,const string& s) const {
	return color_map->bg_color(transform(x),s);
    }

    RGB fg_color(double x,const string& s) const {
	return color_map->fg_color(transform(x),s);
    }

    ColorScheme(const ColorMap& CM,const std::function<double(double)>& S)
	:transform(S),color_map(CM)
	{ }
};

using std::cout;
using std::cerr;
using std::endl;

string getstyle(double d,const string& s,const ColorScheme& color_scheme) 
{
    string style;
    if ((s == "?") or (s == "???")) {
	style += "background: white;";
	style += "color: white;";
    }
    else if ((s == "=") or (s == "===")) {
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


matrix<double> read_alignment_certainty(const vector<sequence>& S, int alignment_length, const string& filename)
{
    checked_ifstream colorfile(filename,"alignment annotation file");

    vector<int> mapping;
    {
	string line;
	portable_getline(colorfile,line);
	vector<string> colornames = split(line,' ');
	vector<string> leafnames;
	for(auto& s: S)
	    leafnames.push_back(s.name);
	mapping = compute_mapping(colornames,leafnames);
    }

    // Add an entry for the ENTIRE COLUMN
    mapping.push_back(mapping.size());

    //------------------ Read in the colors ------------------------//
    matrix<double> colors(alignment_length, S.size()+1);
    for(int column=0;column<colors.size1();column++) 
    { 
	// read a line
	string line;
	portable_getline(colorfile,line);

	// collect the non-empty words
	vector<string> words = split(line,' ');
	for(int i=words.size()-1;i>=0;i--)
	    if (not words[i].size())
		words.erase(words.begin()+i);

	if (words.size() != colors.size2())
	    throw myexception()<<"AU probabilities (column "<<column+1<<"): expected "<<colors.size2()<<" probabilities, but got "<<words.size()<<" words.";

	//TODO - use an istringstream to make things properly per-line
	for(int i=0;i<words.size();i++) {
	    double d = -1;
	    if (words[i] != "-")
		d = convertTo<double>(words[i]);

	    colors(column,mapping[i]) = d;
	}
    }

    string trailing_line;
    while (portable_getline(colorfile, trailing_line))
        if (trailing_line.find_first_not_of(" \t\r") != string::npos)
            throw myexception()<<"AU probabilities contain more than "<<alignment_length
                               <<" alignment columns.";

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
bool match(vector<string>& sstack,const string& s,string& arg) {
    if (not sstack.size())
	return false;
  
    bool success=false;
    string top = sstack.back();

    if (top == s) {
	arg = "";
	success=true;
    }
    else if (top.size() > 1 and top[top.size()-1] == ']') {
	int loc = top.find('[');
	if (loc == -1) return false;
	string name = top.substr(0,loc);
	arg = top.substr(loc+1,top.size()-loc-2);
	success = (name == s);
    }

    if (success)
	sstack.pop_back();
    return success;
}

/// Take something off the string stack, if its present
bool match2(vector<string>& sstack,const string& s) {
    bool m = false;
    if (sstack.size() and sstack.back() == s) {
	m = true;
	sstack.pop_back();
    }
    return m;
}


std::function<double(double)> get_scale(const variables_map& args)
{
    string scale_name = args["scale"].as<string>();
    if (scale_name == "identity") 
	return [](double x){return x;};

    Scale scale;
    if (scale_name == "invert") {
	scale.f = invert;
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

owned_ptr<ColorMap> get_base_color_map(vector<string>& string_stack,bool gaps_different)
{
    owned_ptr<ColorMap> color_map;
    string arg;
    if (match(string_stack,"plain",arg))
	color_map = claim(new Plain_ColorMap);
    else if (match(string_stack,"bw",arg))
	color_map = claim(new BW_ColorMap);
    else if (match(string_stack,"RedBlue",arg))
	color_map = claim(new Rainbow_ColorMap(0.7,1,gaps_different));
    else if (match(string_stack,"BlueRed",arg))
	color_map = claim(new Rainbow_ColorMap(1,0.7,gaps_different));
    else if (match(string_stack,"diff",arg))
    {
	color_map = claim(new Plain_ColorMap);
	vector<double> v = convertTo<double>(split(arg,','));
	if (v.size() == 1)
	    color_map = claim(new Diff_ColorMap((int)v[0], gaps_different));
	else if (v.size() == 3)
	    color_map = claim(new Diff_ColorMap((int)v[0], v[1], v[2], gaps_different));
	else
	    throw myexception()<<"diff: argument should be for the form [int] or [int,double,double]";
    }
    else if (match(string_stack,"Rainbow",arg)) 
    {
	double start = 0.666;
	double end = 0;
	if (arg != "") {
	    vector<double> v = convertTo<double>(split(arg,','));
	    if (v.size() != 2)
		throw myexception()<<"Rainbow: argument should be for the form [start,end]";
	    start = v[0];
	    end = v[1];
	}

	color_map = claim(new Rainbow_ColorMap(start,end,gaps_different));
    }
    else if (match(string_stack,"AA",arg))
	color_map = claim(new AA_colors);
    else if (match(string_stack,"DNA",arg))
	color_map = claim(new DNA_colors);
    else if (match(string_stack,"RNA",arg))
	color_map = claim(new DNA_colors);
    else
	throw myexception()<<"Unrecognized base color scheme '"<<string_stack.back()<<"'";

    return color_map;
}


owned_ptr<ColorMap> get_color_map(const variables_map& args,bool gaps_different) 
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
  
    owned_ptr<ColorMap> color_map = get_base_color_map(string_stack,gaps_different);

    string arg;
    while(string_stack.size()) {
	if (match(string_stack,"switch",arg))
	    color_map = switch_fg_bg(*color_map);
	else if (match(string_stack,"contrast",arg))
	    color_map = contrast(*color_map);
	else if (match(string_stack,"fade",arg)) {
	    if (arg == "")
		color_map = whiten_colors(*color_map,0.2,0.3);
	    else {
		vector<double> min = convertTo<double>(split(arg,','));
		if (min.size() == 1)
		    color_map = whiten_colors(*color_map,0.2,min[0]);
		else if (min.size() == 2)
		    color_map = whiten_colors(*color_map,min[0],min[1]);
		else
		    throw myexception()<<"fade: argument '"<<arg<<"' not recognized.";
	    }
	}
	else
	    throw myexception()<<"Unrecognized color scheme modifier '"<<string_stack.back()<<"'";
    }
    return color_map;
}

owned_ptr<ColorScheme> get_color_scheme(const variables_map& args) 
{
    bool gaps_different = false;
    if (args.count("gaps-different") and args["gaps-different"].as<string>() == "yes")
	gaps_different = true;

    owned_ptr<ColorMap> color_map = get_color_map(args,gaps_different);
    assert(color_map);

    auto transform = get_scale(args);

    return ColorScheme(*color_map,transform);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description general("General options");
    general.add_options()
	("help,h", "produce help message")
	("file", value<string>(),"file with sequences and initial alignment")
	;
  
    options_description output("Output");
    output.add_options()
	("legend","Print a legend showing how color-scheme indicates uncertainty.") 
	("show-ruler","Print a ruler to show column numbers") 
	("column-colors","Color-code column ticks by column certainty") 
	("AU",value<string>(),"file with alignment uncertainties")
	("properties",value<string>(),"JSON file with posterior character-property summaries")
	("alphabet",value<string>(),"alphabet name used to tokenize alignment cells")
	("show-gaps",value<string>()->default_value("yes"),"Show gaps")
	("show-letters",value<string>()->default_value("yes"),"Show letters")
	("show-names",value<string>()->default_value("yes"),"Show names")
	("gaps-different",value<string>()->default_value("yes"),"Color gaps in grey.")
	("width",value<int>(),"The number of columns per line")
	("start",value<int>(),"The first column to plot")
	("end",value<int>(),"The last column to plot")
	("format",value<string>()->default_value("HTML"),"produce a plot in this format")
	("min",value<double>(),"Minimum value of scale function")
	("max",value<double>(),"Maximum value of scale function")
	("color-scheme",value<string>(),"Include a length of how certainties map to colors")
	("scale",value<string>()->default_value("LOD"),"scale for the uncertainties")
	;

    options_description all("All options");
    all.add(general).add(output);

    // positional options
    positional_options_description p;
    p.add("file", 1);
    p.add("AU", 2);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    notify(args);

    if (args.count("help")) {
	cout<<"Draw an alignment to HTML, optionally coloring residues by AU.\n\n";
	cout<<"Usage: alignment-draw <alignment> [<AU file>] [OPTIONS]\n\n";
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


vector<sequence> load_file(std::istream& file)
{
    vector<sequence> s = sequence_format::read_guess(file);
    if (s.size() == 0)
	throw myexception()<<"Alignment didn't contain any sequences!";

    pad_to_same_length(s);

    return s;
}

vector<sequence> load_file(const string& filename)
{
    checked_ifstream file(filename,"alignment file");
    try{
        return load_file(file);
    }
    catch (myexception& e)
    {
        e.prepend("File '"+filename+"': ");
        throw;
    }
}

struct alignment_token
{
    int alphabet_code;
    int character_index;
};

struct tokenized_alignment
{
    int token_width;
    vector<vector<alignment_token>> rows;

    const vector<alignment_token>& operator[](size_t index) const {return rows[index];}
};

/// Split every alignment row into alphabet-width cells and attach ungapped coordinates.
tokenized_alignment tokenize_alignment(const vector<sequence>& sequences, const alphabet* alph)
{
    int token_width = alph ? alph->width() : 1;
    vector<vector<alignment_token>> token_rows;
    std::optional<size_t> n_columns;

    for (const auto& seq: sequences)
    {
        vector<int> codes;
        if (alph)
            codes = (*alph)(static_cast<const string&>(seq));
        else
        {
            codes.reserve(seq.size());
            for (char letter: seq)
            {
                if (letter == '-')
                    codes.push_back(alphabet::gap);
                else if (letter == '?' or letter == '=')
                    codes.push_back(alphabet::unknown);
                else
                    codes.push_back(0);
            }
        }

        if (not n_columns)
            n_columns = codes.size();
        else if (codes.size() != *n_columns)
            throw myexception()<<"Sequence '"<<seq.name<<"' has "<<codes.size()
                               <<" alignment cells, but the first sequence has "<<*n_columns<<".";

        vector<alignment_token> tokens;
        tokens.reserve(codes.size());
        int character_index = 0;
        for (size_t column = 0; column < codes.size(); column++)
        {
            int code = codes[column];
            int index = -1;
            if (alphabet::is_feature(code))
                index = character_index++;
            tokens.push_back({code, index});
        }
        token_rows.push_back(std::move(tokens));
    }

    if (not n_columns or *n_columns == 0)
        throw myexception()<<"Alignment did not contain any character columns.";
    return {token_width, std::move(token_rows)};
}

/// Return a required object field, adding context to errors from malformed schemas.
const json::value& required_json_field(const json::object& object, const char* field, const string& context)
{
    if (const auto* value = object.if_contains(field))
        return *value;
    throw myexception()<<context<<": missing required field '"<<field<<"'.";
}

/// Read a JSON string field while reporting the field name on type errors.
string required_json_string(const json::object& object, const char* field, const string& context)
{
    const auto& value = required_json_field(object, field, context);
    if (not value.is_string())
        throw myexception()<<context<<": field '"<<field<<"' must be a string.";
    return string(value.as_string().c_str());
}

/// Convert a JSON integer to an unsigned count without accepting negative values.
uint64_t json_count(const json::value& value, const string& context)
{
    if (value.is_uint64())
        return value.as_uint64();
    if (value.is_int64() and value.as_int64() >= 0)
        return value.as_int64();
    throw myexception()<<context<<" must be a non-negative integer.";
}

/// Convert any JSON number kind to a finite double for property validation.
double finite_json_number(const json::value& value, const string& context)
{
    double result;
    if (value.is_double())
        result = value.as_double();
    else if (value.is_int64())
        result = value.as_int64();
    else if (value.is_uint64())
        result = value.as_uint64();
    else
        throw myexception()<<context<<" must be a number or null.";

    if (not std::isfinite(result))
        throw myexception()<<context<<" must be finite.";
    return result;
}

/// Validate paired mean/count arrays, with an exact length for displayed sequences.
void validate_property_sequence(const string& property_context,
                                const string& sequence_name,
                                const json::value& means_value,
                                const json::value& counts_value,
                                std::optional<size_t> displayed_character_count,
                                uint64_t retained_samples)
{
    if (not means_value.is_array())
        throw myexception()<<property_context<<": mean values for sequence '"<<sequence_name<<"' must be an array.";
    if (not counts_value.is_array())
        throw myexception()<<property_context<<": count values for sequence '"<<sequence_name<<"' must be an array.";

    const auto& means = means_value.as_array();
    const auto& counts = counts_value.as_array();
    if (means.size() != counts.size())
        throw myexception()<<property_context<<": sequence '"<<sequence_name
                           <<"' has different numbers of mean and count values.";
    if (displayed_character_count and means.size() != *displayed_character_count)
        throw myexception()<<property_context<<": sequence '"<<sequence_name<<"' has "<<means.size()
                           <<" values, but the alignment has "<<*displayed_character_count<<" characters.";

    for (size_t character = 0; character < means.size(); character++)
    {
        string cell_context = property_context+", sequence '"+sequence_name+"', character "+convertToString(character);
        uint64_t count = json_count(counts[character], cell_context+" count");
        if (count > retained_samples)
            throw myexception()<<cell_context<<" count exceeds retained_samples.";
        if (means[character].is_null())
        {
            if (count != 0)
                throw myexception()<<cell_context<<" has a null mean but a nonzero count.";
        }
        else
        {
            finite_json_number(means[character], cell_context+" mean");
            if (count == 0)
                throw myexception()<<cell_context<<" has a numeric mean but a zero count.";
        }
    }
}

/// Check one named property's arrays and require every displayed sequence.
void validate_property(const string& property_name,
                       const json::value& property_value,
                       const std::map<string,size_t>& character_counts,
                       uint64_t retained_samples)
{
    string context = "Property '"+property_name+"'";
    if (not property_value.is_object())
        throw myexception()<<context<<" must be an object.";
    const auto& property = property_value.as_object();

    const auto& mean_value = required_json_field(property, "mean", context);
    const auto& count_value = required_json_field(property, "count", context);
    if (not mean_value.is_object())
        throw myexception()<<context<<": field 'mean' must be an object keyed by sequence name.";
    if (not count_value.is_object())
        throw myexception()<<context<<": field 'count' must be an object keyed by sequence name.";
    const auto& means = mean_value.as_object();
    const auto& counts = count_value.as_object();

    if (means.size() != counts.size())
        throw myexception()<<context<<": fields 'mean' and 'count' contain different sequence names.";

    for (const auto& item: means)
    {
        string sequence_name(item.key_c_str());
        const auto* sequence_counts = counts.if_contains(item.key());
        if (not sequence_counts)
            throw myexception()<<context<<": count values are missing sequence '"<<sequence_name<<"'.";
        std::optional<size_t> displayed_character_count;
        if (auto displayed = character_counts.find(sequence_name); displayed != character_counts.end())
            displayed_character_count = displayed->second;
        validate_property_sequence(context, sequence_name, item.value(), *sequence_counts,
                                   displayed_character_count, retained_samples);
    }

    for (const auto& entry: character_counts)
        if (not means.if_contains(entry.first))
            throw myexception()<<context<<": mean values are missing sequence '"<<entry.first<<"'.";
}

/// Parse and fully validate the reviewed character-property interchange format.
json::value read_character_properties(const string& filename,
                                      const vector<sequence>& sequences,
                                      const tokenized_alignment& tokens)
{
    checked_ifstream input(filename, "character property file");
    std::ostringstream contents;
    contents<<input.rdbuf();

    json::value document;
    try {
        document = json::parse(contents.str());
    }
    catch (const std::exception& error) {
        throw myexception()<<"Character property file '"<<filename<<"': invalid JSON: "<<error.what();
    }
    if (not document.is_object())
        throw myexception()<<"Character property file '"<<filename<<"' must contain a JSON object.";
    const auto& root = document.as_object();
    string context = "Character property file '"+filename+"'";

    if (required_json_string(root, "format", context) != "bali-phy-character-properties")
        throw myexception()<<context<<": unrecognized format.";
    if (json_count(required_json_field(root, "version", context), context+" version") != 1)
        throw myexception()<<context<<": only version 1 is supported.";

    const auto& coordinates_value = required_json_field(root, "coordinates", context);
    if (not coordinates_value.is_object())
        throw myexception()<<context<<": field 'coordinates' must be an object.";
    const auto& coordinates = coordinates_value.as_object();
    if (required_json_string(coordinates, "kind", context+" coordinates") != "ungapped-sequence-character")
        throw myexception()<<context<<": unsupported coordinate kind.";
    if (json_count(required_json_field(coordinates, "index_base", context+" coordinates"),
                   context+" coordinates index_base") != 0)
        throw myexception()<<context<<": coordinates index_base must be 0.";

    const auto& selection = required_json_field(root, "selection", context);
    if (not selection.is_object())
        throw myexception()<<context<<": field 'selection' must be an object.";
    const auto& selection_object = selection.as_object();
    string selection_context = context+" selection";
    std::optional<uint64_t> skip;
    std::optional<uint64_t> until;
    const auto& skip_value = required_json_field(selection_object, "skip", selection_context);
    const auto& until_value = required_json_field(selection_object, "until", selection_context);
    if (not skip_value.is_null())
        skip = json_count(skip_value, context+" selection skip");
    if (not until_value.is_null())
        until = json_count(until_value, context+" selection until");
    uint64_t subsample = json_count(
        required_json_field(selection_object, "subsample", selection_context),
        context+" selection subsample");
    if (subsample == 0)
        throw myexception()<<context<<": selection subsample must be positive.";
    if (skip and until and *until <= *skip)
        throw myexception()<<context<<": selection until must be greater than skip.";

    uint64_t retained_samples = json_count(required_json_field(root, "retained_samples", context),
                                           context+" retained_samples");
    const auto& chain_counts_value = required_json_field(root, "retained_samples_by_chain", context);
    if (not chain_counts_value.is_array())
        throw myexception()<<context<<": field 'retained_samples_by_chain' must be an array.";
    uint64_t chain_total = 0;
    for (const auto& count: chain_counts_value.as_array())
        chain_total += json_count(count, context+" retained_samples_by_chain entry");
    if (chain_total != retained_samples)
        throw myexception()<<context<<": retained_samples_by_chain does not sum to retained_samples.";

    std::map<string,size_t> character_counts;
    for (size_t sequence_index = 0; sequence_index < sequences.size(); sequence_index++)
    {
        size_t count = 0;
        for (const auto& token: tokens[sequence_index])
            if (token.character_index >= 0)
                count++;
        if (not character_counts.emplace(sequences[sequence_index].name, count).second)
            throw myexception()<<"Alignment contains duplicate sequence name '"<<sequences[sequence_index].name<<"'.";
    }

    const auto& properties_value = required_json_field(root, "properties", context);
    if (not properties_value.is_object())
        throw myexception()<<context<<": field 'properties' must be an object.";
    for (const auto& item: properties_value.as_object())
        validate_property(string(item.key_c_str()), item.value(), character_counts, retained_samples);

    return document;
}

/// Escape text inserted into HTML text and attribute contexts.
string escape_html(std::string_view text)
{
    string escaped;
    escaped.reserve(text.size());
    for (char c: text)
    {
        if (c == '&') escaped += "&amp;";
        else if (c == '<') escaped += "&lt;";
        else if (c == '>') escaped += "&gt;";
        else if (c == '"') escaped += "&quot;";
        else if (c == '\'') escaped += "&#39;";
        else escaped += c;
    }
    return escaped;
}

/// Serialize JSON for a script data block without permitting an HTML end tag.
string serialize_json_for_html(const json::value& document)
{
    string serialized = json::serialize(document);
    string escaped;
    escaped.reserve(serialized.size());
    for (char c: serialized)
    {
        if (c == '<') escaped += "\\u003c";
        else if (c == '>') escaped += "\\u003e";
        else if (c == '&') escaped += "\\u0026";
        else escaped += c;
    }
    return escaped;
}

/// Package scientific property data, display order, and optional grid-cell AU values.
json::value make_viewer_payload(const json::value& character_properties,
                                const vector<sequence>& sequences,
                                const matrix<double>& colors,
                                bool include_uncertainty)
{
    json::object payload;
    payload["format"] = "bali-phy-alignment-viewer";
    payload["version"] = 1;

    json::array sequence_names;
    for (const auto& seq: sequences)
        sequence_names.push_back(json::string(seq.name));
    payload["sequences"] = std::move(sequence_names);
    payload["character_properties"] = character_properties;

    if (include_uncertainty)
    {
        for (int column = 0; column < colors.size1(); column++)
            for (int sequence = 0; sequence < colors.size2(); sequence++)
            {
                double value = colors(column, sequence);
                if (value >= 0 and (not std::isfinite(value) or value > 1))
                    throw myexception()<<"AU value at column "<<column+1
                                       <<" is not a probability in [0,1].";
            }

        json::object uncertainty;
        uncertainty["kind"] = "posterior-alignment-probability";
        uncertainty["coordinates"] = {
            {"kind", "alignment-grid-cell"},
            {"index_base", 0}
        };

        json::array rows;
        for (size_t sequence = 0; sequence < sequences.size(); sequence++)
        {
            json::array row;
            for (int column = 0; column < colors.size1(); column++)
            {
                double value = colors(column, sequence);
                if (value < 0)
                    row.push_back(nullptr);
                else
                    row.push_back(value);
            }
            rows.push_back(std::move(row));
        }
        uncertainty["mean"] = std::move(rows);
        payload["alignment_uncertainty"] = std::move(uncertainty);
    }

    return payload;
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

	bool show_gaps = true;
	if (args.count("show-gaps") and args["show-gaps"].as<string>() == "no")
	    show_gaps = false;

	bool show_letters = true;
	if (args.count("show-letters") and args["show-letters"].as<string>() == "no")
	    show_letters = false;

	bool show_names = true;
	if (args.count("show-names") and args["show-names"].as<string>() == "no")
	    show_names = false;

	//---------- Load alignment and tree -----------//
        vector<sequence> S;
	if (not args.count("file"))
	    throw myexception()<<"No filename given!";

        auto filename = args["file"].as<string>();
        if (filename == "-")
            S = load_file(std::cin);
        else
            S = load_file(filename);

        int n_sequences = S.size();

	std::shared_ptr<const alphabet> alph;
	if (args.count("alphabet"))
	    alph = get_alphabet(args["alphabet"].as<string>());
	if (args.count("properties") and not alph)
	    throw myexception()<<"Option '--alphabet' is required when '--properties' is supplied.";
	if (args.count("properties") and args["format"].as<string>() != "HTML")
	    throw myexception()<<"Character properties are currently supported only for HTML output.";

	auto tokens = tokenize_alignment(S, alph.get());
        int L = tokens[0].size();

	std::optional<json::value> character_properties;
	if (args.count("properties"))
	    character_properties = read_character_properties(args["properties"].as<string>(), S, tokens);

	//---- Find mapping from colorfile to alignment sequence order -----//
	owned_ptr<ColorScheme> color_scheme = get_color_scheme(args);

	//---------- Read alignment uncertainty, if available --------------//
	matrix<double> colors(L, n_sequences+1);
    
	if (args.count("AU"))
	    colors = read_alignment_certainty(S,L,args["AU"].as<string>());
	else
	    for(int i=0;i<colors.size1();i++)
		for(int j=0;j<colors.size2();j++)
		    colors(i,j) = -1;

	//-------------------- Get width -----------------------//
	int width = 2*L;
	if (args.count("width"))
	    width = args["width"].as<int>();

	//-------------------- Get start -----------------------//
	int start=0;
	if (args.count("start")) {
	    start = args["start"].as<int>();
	    if (start < 0)
		throw myexception()<<"Parameter 'start' must be positive.";
	    if (not (start < L))
		throw myexception()<<"Parameter 'start' must be less than the length of the alignment ("<<L<<").";
	}
    
	//--------------------- Get end ------------------------//
	int end = L-1;
	if (args.count("end")) {
	    end =args["end"].as<int>();
	    if (end < 0)
		throw myexception()<<"Parameter 'end' must be positive.";
	    if (not (end < L))
		throw myexception()<<"Parameter 'end' must be less than the length of the alignment ("<<L<<").";
	    if (end < start)
		throw myexception()<<"Parameter 'end' must be >= than parameter 'start'"<<L<<").";
	}

	if (args["format"].as<string>() == "latex") {
	    std::cout<<"\\documentclass[10pt]{article}\n\
\\usepackage{colortbl}\n\
\\begin{document}\n\
\\definecolor{c2}{rgb}{1.0,0.5,0.0}\n";

	    /*-------------------- Print the alignment ------------------------*/
	    int pos=start;
	    while(pos<=end) {
		cout<<"\\begin{tabular}{";
		for(int i=0;i<width;i++)
		    cout<<"c";
		cout<<"}\n";
	
		for(int i=0;i<S.size();i++) {
		    int s = i;

		    for(int column=pos;column<pos+width and column <= end; column++) {
			std::string_view c(static_cast<const string&>(S[s]).data()
			                   + column*tokens.token_width, tokens.token_width);
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
<!doctype html>\n\
<html lang=\"en\">\n\
 <head>\n\
  <meta charset=\"utf-8\" />\n\
  <title>Alignment Uncertainty: "<<escape_html(args["file"].as<string>())<<"</title>\n\
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
   padding-left: 0.005em;\n\
   padding-right: 0.005em;\n\
   padding-top: 0.1em;\n\
   padding-bottom: 0.1em;\n\
   margin: 0;\n\
\n\
   font-size: 12pt; \n\
   font-weight: bold;\n\
   font-family: monospace;\n\
\n\
   white-space: nowrap;\n\
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
}\n";

	    if (character_properties)
		cout<<alignment_draw_stylesheet;

	    cout<<"\
    </style>\n\
  </head>\n\
  <body>\n\n";

	    if (character_properties)
	    {
		auto viewer_payload = make_viewer_payload(
		    *character_properties, S, colors, args.count("AU"));
		cout<<"<script type=\"application/json\" id=\"alignment-viewer-data\">"
		    <<serialize_json_for_html(viewer_payload)<<"</script>\n";
	    }

	    //-------------------- Print a legend ------------------------//
	    if (args.count("legend")) {
		cout<<"\n<table class=\"legend\"><tr>\n";

		cout<<"<td>";
		draw_legend(cout,*color_scheme,"");
		cout<<"</td>\n";

		if (args.count("gaps-different") and args["gaps-different"].as<string>() == "yes") {
		    cout<<"<td>";
		    draw_legend(cout,*color_scheme,"-");
		    cout<<"</td>\n";
		}

		cout<<"</tr></table>\n";
	    }

	    //----------- Compute the position headings -------------------//
	    string positions(L,'.');

	    positions[0]='1';
	    for(int pos=10;pos<L;pos+=10) {
		string position = convertToString(pos);
		const int start = pos - position.size()+1;
		for(int j=0;j<position.size();j++) 
		    positions[start + j]=position[j];
	    }


	    //-------------------- Print the alignment ------------------------//
	    int pos=start;
	    while(pos<=end) {
		cout<<"\n\n<table class=\"sequences\">\n";

		// Print columns positions
		if (show_column_numbers) {
		    cout<<"<tr>";
		    if (show_names)
			cout<<"<td></td>";
	  
		    for(int column=pos;column<pos+width and column <= end; column++) {
			double P=colors(column, S.size());
			string style = getstyle(P,"",*color_scheme);
			if (columncolors)
			    cout<<"<td style=\""<<style<<"\">"<<positions[column]<<"</td>";
			else
			    cout<<"<td>"<<positions[column]<<"</td>";
		    }
		    cout<<"</tr>\n";
		}

		for(int i=0;i<S.size();i++) {
		    int s = i;
		    cout<<"  <tr>\n";
		    if (show_names)
			cout<<"    <td class=\"sequencename\">"<<escape_html(S[s].name)<<"</td>\n";
		    for(int column=pos;column<pos+width and column <= end; column++)
		    {
			const auto& token = tokens[s][column];
			std::string_view c(static_cast<const string&>(S[s]).data()
			                   + column*tokens.token_width, tokens.token_width);
			string c_string = escape_html(c);
			if (not show_letters or (not show_gaps and token.alphabet_code == alphabet::gap))
			    c_string = "&nbsp;";

			// NOTE: Legacy palettes accept one-letter keys; preserve compound gap/missing
			// semantics and use neutral colors until they support compound characters.
			string color_key(c);
			if (c.size() > 1)
			{
			    if (token.alphabet_code == alphabet::gap) color_key = "-";
			    else if (token.alphabet_code == alphabet::unknown) color_key = "?";
			    else color_key = "";
			}
			string style = getstyle(colors(column,s),color_key,*color_scheme);
			cout<<"<td";
			if (character_properties)
			    cout<<" class=\"alignment-cell\" data-sequence=\""<<s
				<<"\" data-column=\""<<column
				<<"\" data-character=\""<<token.character_index<<"\"";
			cout<<" style=\""<<style<<"\">"<<c_string<<"</td>";
		    }
		    cout<<"  </tr>\n";
		}
		cout<<"<tr>";
		if (show_names) cout<<"<td></td>";
		cout<<"<td>&nbsp;</td></tr>\n";
		cout<<"</table>"<<endl;
		pos += width;
	    }
	    if (character_properties)
		cout<<"<script>\n"<<alignment_draw_javascript<<"\n</script>\n";
	    cout<<"</body>\n</html>\n";
	}
    }
    catch (std::exception& e) {
	std::cerr<<"alignment-draw: Error! "<<e.what()<<endl;
	return 1;
    }
    return 0;

}


//FIXME - IMPLEMENT
// EPS output - use libplot - unmaintained for 4 years?
// make a STACK of things which modify the previous one - e.g. whiten the color
//    - We still need a bottom layer
// can we make the cutoff (e.g. min) different for hue and saturation?
//    - It seems that hsv is a good model - if plugins modify it, then different 
//      plugins could have different mins. (But it should perhaps be independent of 
//      the "scale" used.)

// plugins:
//   : color-gaps-differently.
//   : whiten-below-0.5
//   : fore-ground-colors (AA)
//   : whiten-according to uncertainty
