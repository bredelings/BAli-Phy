#include <cmath>
#include <string>
#include <iostream>
#include <vector>
#include <fstream>

#include <cairo.h>
#include <cairo-ps.h>
#include <cairo-pdf.h>

#include <boost/program_options.hpp>

#include "myexception.H"
#include "sequencetree.H"
#include "tree-dist.H"

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

const double inch = 72.0;

void draw_curvy_rectangle(cairo_t* cr)
{
  // a custom shape, that could be wrapped in a function 
  double x0	   = 0.1;   /*< parameters like cairo_rectangle */
  double y0	   = 0.1;
  double rect_width  = 0.8;
  double rect_height = 0.8;
  double radius = 0.4;   /*< and an approximate curvature radius */

  cairo_scale(cr, 8.5*inch, 11*inch);
  cairo_set_line_width(cr, 0.04);
  
  double x1 = x0 + rect_width;
  double y1 = y0 + rect_height;

  if (!rect_width || !rect_height)
    return;
  if (rect_width/2<radius) {
    if (rect_height/2<radius) {
      cairo_move_to  (cr, x0, (y0 + y1)/2);
      cairo_curve_to (cr, x0 ,y0, x0, y0, (x0 + x1)/2, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, (y0 + y1)/2);
      cairo_curve_to (cr, x1, y1, x1, y1, (x1 + x0)/2, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, (y0 + y1)/2);
    } else {
      cairo_move_to  (cr, x0, y0 + radius);
      cairo_curve_to (cr, x0 ,y0, x0, y0, (x0 + x1)/2, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
      cairo_line_to (cr, x1 , y1 - radius);
      cairo_curve_to (cr, x1, y1, x1, y1, (x1 + x0)/2, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- radius);
    }
  } else {
    if (rect_height/2<radius) {
      cairo_move_to  (cr, x0, (y0 + y1)/2);
      cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
      cairo_line_to (cr, x1 - radius, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, (y0 + y1)/2);
      cairo_curve_to (cr, x1, y1, x1, y1, x1 - radius, y1);
      cairo_line_to (cr, x0 + radius, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, (y0 + y1)/2);
    } else {
      cairo_move_to  (cr, x0, y0 + radius);
      cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
      cairo_line_to (cr, x1 - radius, y0);
      cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
      cairo_line_to (cr, x1 , y1 - radius);
      cairo_curve_to (cr, x1, y1, x1, y1, x1 - radius, y1);
      cairo_line_to (cr, x0 + radius, y1);
      cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- radius);
    }
  }
  cairo_close_path (cr);
  
  cairo_set_source_rgb (cr, 0.5,0.5,1);
  cairo_fill_preserve (cr);
  // it seems that alpha blending results in a very large image ;S
  //  cairo_set_source_rgba (cr, 0.5, 0, 0, 0.5);
  cairo_set_source_rgb (cr, 0.5,0.0,0);
  cairo_stroke (cr);
}

void draw_to_page(cairo_surface_t* surface,void (*draw)(cairo_t*))
{
  cairo_t *cr = cairo_create(surface);

  draw(cr);
  cairo_show_page(cr);

  cairo_surface_destroy(surface);
  cairo_destroy(cr);
}

void draw_to_ps(const string& filename,void (*draw)(cairo_t*))
{
  cairo_surface_t *surface = cairo_ps_surface_create(filename.c_str(),8*72.0, 11*72.0);

  draw_to_page(surface,draw);
}

void draw_to_pdf(const string& filename,void (*draw)(cairo_t*))
{
  cairo_surface_t *surface = cairo_pdf_surface_create(filename.c_str(),8*72.0, 11*72.0);

  draw_to_page(surface,draw);
}

variables_map parse_cmd_line(int argc,char* argv[])
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("file",value<string>(),"predicates to examine")
    ;
  
  options_description all("All options");
  all.add(input);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: draw-graph <file1>\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("file"))
    throw myexception()<<"No file supplied.";

  return args;
}

class MC_tree
{
public:
  vector<Partition>  branches;
  vector<double> branch_lengths;

  vector<Partition> nodes;
  vector<double> node_lengths;

  vector<Partition> mini_branches;
  vector<double> mini_branch_lengths;
  vector<int> parent_branch_of_mini_branch;
};

/// FIXME - check partitions to see that names are all unique?

MC_tree get_MC_tree(const string& filename)
{
  MC_tree MC;
  std::ifstream file(filename.c_str());

  if (not file)
    throw myexception()<<"Can't open file '"<<filename<<"'";

  string line;
  while(file) {
    while (getline(file,line) and not line.size());
    if (not file) break;
    vector<string> words = split(line,' ');

    if (words.size() != 2)
      throw myexception()<<"Don't understand line '"<<line<<"'";

    double length = convertTo<double>(words[1]);

    if (not getline(file,line))
      throw myexception()<<"Missing partition at end of file!";

    if (not line.size())
      throw myexception()<<"Empty line in middle of file?";

    Partition P(line);
    line = "";
    if (words[0] == "branch") {
      MC.branches.push_back(P);
      MC.branch_lengths.push_back(length);
    }
    else if (words[0] == "node") {
      MC.nodes.push_back(P);
      MC.node_lengths.push_back(length);
    }
    else if (words[0] == "mini-branch") {
      MC.mini_branches.push_back(P);
      MC.mini_branch_lengths.push_back(length);
    }
    else
      throw myexception()<<"Don't understand word="<<words[0];;
  }

  cerr<<"Read "<<MC.branches.size()<<" partitions."<<endl;
  return MC;
}

SequenceTree build_tree(const MC_tree& MC)
{
  vector<Partition> full_partitions;
  for(int i=0;i<MC.branches.size();i++)
    if (MC.branches[i].full())
      full_partitions.push_back(MC.branches[i]);

  cerr<<"Read "<<MC.branches.size()<<" partitions ... "<<full_partitions.size()<<" full partitions."<<endl;

  SequenceTree MF = star_tree(full_partitions[0].names);
  for(int i=0;i<full_partitions.size();i++) {
    cerr<<"i = "<<i<<"    pi = "<<full_partitions[i]<<endl;
    MF.induce_partition(full_partitions[i].group1);
  }

  //set the branch lengths!

  cout<<MF.write(true)<<endl;

  return MF;
}


int main(int argc,char* argv[]) 
{
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //-------- Load MC Tree and lengths -----//
    MC_tree MC = get_MC_tree(args["file"].as<string>());

    SequenceTree MF = build_tree(MC);

    // do we need a generic draw-to-cr-context object?

    // lay out the tree

    // draw the tree

    draw_to_ps("tree.ps",draw_curvy_rectangle);
    draw_to_pdf("tree.pdf",draw_curvy_rectangle);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

