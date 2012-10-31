/*
   Copyright (C) 2007-2010 Benjamin Redelings

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
#include <string>
#include <iostream>
#include <vector>
#include <fstream>

// FIXME - Add a command-line option to turn on clouds.
//       - Actually draw arrows on type 2 edges.
//       - Figure out how to use boost's Kamada-Kawai layout.
//       - Change the coloring algorithm to find conflicts based on shared NODES.

// FIXME - the energy_layout2 changes are still not incorporated.

#include <cairo.h>
#include <cairo-ps.h>
#include <cairo-pdf.h>
#include <cairo-svg.h>

#include <boost/program_options.hpp>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_FENV_H
extern "C" {
#include "fenv.h"
}
#endif

#include "myexception.H"
#include "tree/sequencetree.H"
#include "pow2.H"
#include "mctree.H"
#include "util.H"
#include "util-random.H"
#include "rng.H"
#include "io.H"

namespace po = boost::program_options;
using po::variables_map;

const double inch = 72.0;

using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::ostream;
using std::abs;

class cairo_plotter
{
  double page_x_width_;
  double page_y_width_;
  double font_size_;
public:
  virtual void operator()(cairo_t*) = 0;

  double page_x_width() const {return page_x_width_;}
  double page_y_width() const {return page_y_width_;}

  double font_size() const {return font_size_;}

  cairo_plotter(double xw, double yw)
    :page_x_width_(xw),
     page_y_width_(yw),
     font_size_(10)
  {}

  cairo_plotter(double xw, double yw, double fs)
    :page_x_width_(xw),
     page_y_width_(yw),
     font_size_(fs)
  {}
  virtual ~cairo_plotter() {}
};

struct fn_plotter: public cairo_plotter
{
  void (*f)(cairo_t*);
  void operator()(cairo_t* cr) {f(cr);}
  fn_plotter(void (*f1)(cairo_t*),double xw, double yw)
    :cairo_plotter(xw,yw),
     f(f1)
  {}

};

void draw_to_page(cairo_surface_t* surface, cairo_plotter& draw)
{
  cairo_t *cr = cairo_create(surface);

  draw(cr);
  cairo_show_page(cr);

  cairo_surface_destroy(surface);
  cairo_destroy(cr);
}

void draw_to_ps(const string& filename, cairo_plotter& draw)
{
  cairo_surface_t *surface = cairo_ps_surface_create(filename.c_str(),
						     draw.page_x_width()*inch, 
						     draw.page_y_width()*inch);

  draw_to_page(surface,draw);
}

void draw_to_pdf(const string& filename, cairo_plotter& draw)
{
  cairo_surface_t *surface = cairo_pdf_surface_create(filename.c_str(),
						      draw.page_x_width()*inch,
						      draw.page_y_width()*inch);

  draw_to_page(surface,draw);
}

void draw_to_svg(const string& filename, cairo_plotter& draw)
{
  cairo_surface_t *surface = cairo_svg_surface_create(filename.c_str(),
						      draw.page_x_width()*inch,
						      draw.page_y_width()*inch);

  draw_to_page(surface,draw);
}

variables_map parse_cmd_line(int argc,char* argv[])
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "Produce help message")
    ("width,w",value<double>()->default_value(8.5),"Page width in inches")
    ("height,h",value<double>()->default_value(11),"Page height in inches")
    ("file",value<string>(),"predicates to examine")
    ("output",value<string>()->default_value("pdf"),"Type of output to write: tree, topology, mtree, lengths, dot, ps, pdf, svg")
    ("out",value<string>(),"Output filename (without extension)")
    ("full","Consider only full splits by collapsing any partial splits.")
    ("iterations",value<int>()->default_value(2),"Number of iterations for layout algorithm")
    ("font-size",value<double>()->default_value(10),"Font size for taxon names")
    ("angle_iterations",value<int>()->default_value(0),"Number of iterations for layout algorithm with small-angle penalties")
    ("labels",value<string>()->default_value("horizontal"),"Are the names horizontal or angled?")
    ("collapse","Give node lengths evenly to neighboring branches and zero the node lengths.")
    ("layout",value<string>()->default_value("graph"),"Layout method: graph, equal-angle, equal-daylight, etc.")
    ("greedy","For equal-daylight layout: take as much daylight as possible?")
    ("tree-layout-initial","Start an energy layout with positions from the equal-angle layout - only for multifurcating trees")
    ("no-shade","For equal-daylight layout: reject rotations that shade other groups?")
    ("draw-clouds",value<string>(),"Draw wandering-ranges in MC trees as clouds.")
    ("seed", value<unsigned long>(),"Random seed")
    ("verbose","Output more log messages on stderr.")
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
    cout<<"Usage: draw-tree [OPTIONS] <tree file>\n";
    cout<<"Draw NEWICK (and some other) formatted files.\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("file"))
    throw myexception()<<"No file supplied.";

  if (args.count("verbose")) log_verbose = 1;

  return args;
}


/// FIXME - name collision!
class MC_tree_with_lengths: public MC_tree
{
  vector<double> branch_lengths;
  vector<double> node_lengths;
public:

  double  branch_length(int b) const {return branch_lengths[b];}
  double& branch_length(int b)       {return branch_lengths[b];}

  double  node_length(int n) const {return node_lengths[n];}
  double& node_length(int n)       {return node_lengths[n];}

  double total_length() const;
  void scale(double s);

  //  vector<Partition> mini_branches;
  //  vector<double> mini_branch_lengths;
  //  vector<int> parent_branch_of_mini_branch;
  MC_tree_with_lengths(const vector<Partition>& branches)
    :MC_tree(branches),
     branch_lengths(2*n_branches(),0.0),
     node_lengths(n_nodes(),0.0)
  { }
};

double MC_tree_with_lengths::total_length() const
{
  //determine total length
  double t = 0;
  for(int i=0;i<branch_order.size();i++) 
  {
    int b = branch_order[i];
    t += branch_length(b);
  }
  for(int n=0;n<n_nodes();n++) 
    t += node_length(n);

  return t;
}


void MC_tree_with_lengths::scale(double s)
{
  for(int b=0;b<2*n_branches();b++) 
    branch_length(b) *= s;

  for(int n=0;n<n_nodes();n++) 
    node_length(n) *= s;

  for(int b=0;b<T.n_branches();b++)
    T.branch(b).set_length(T.branch(b).length() * s);
}


/// FIXME - check partitions to see that names are all unique?

MC_tree_with_lengths get_MC_tree_with_lengths(const string& filename)
{
  vector<Partition>  branches;
  vector<double> branch_lengths;
  
  vector<Partition> nodes;
  vector<double> node_lengths;

  vector<Partition> mini_branches;
  vector<double> mini_branch_lengths;
  vector<int> parent_branch_of_mini_branch;

  //-------------------- Read lengths from file --------------------//
  checked_ifstream file(filename,"multi-connected tree file");

  string line;

  bool with_lengths = true;
  bool first_line = true;

  while(file) 
  {
    while (getline(file,line) and not line.size()) {}
    if (not line.size()) break;

    if (first_line and line[0] == '(') 
    {
      with_lengths=false;
      SequenceTree T = standardized(line);
      for(int b=0;b<T.n_branches();b++) {
	Partition P = partition_from_branch(T,b);
	double L = T.branch(b).length();
	if (L < 0)
	  L = 1;
	else
	  with_lengths = true;
	branches.push_back(P);
	branch_lengths.push_back(L);
      }
      first_line=false;
      continue;
    }
    first_line=false;

    vector<string> words = split(line,' ');
    if (words.size() == 2 and words[0] == "branch")
    {
      // get the next line
      if (not file or not getline(file,line))
	throw myexception()<<"Missing partition after 'branch'!";

      if (not line.size())
	throw myexception()<<"Empty line after 'branch'!";

      double length = convertTo<double>(words[1]);
      Partition P(line);

      branches.push_back(P);
      branch_lengths.push_back(length);
    }
    else if (words.size() == 2 and words[0] == "node")
    {
      // get the next line
      if (not file or not getline(file,line))
	throw myexception()<<"Missing partition after 'node'!";

      if (not line.size())
	throw myexception()<<"Empty line after 'node'!";

      double length = convertTo<double>(words[1]);
      Partition P(line);

      assert(not isnan(length));

      nodes.push_back(P);
      node_lengths.push_back(length);
    }
    else if (words.size() == 2 and words[0] == "mini-branch")
    {
      // get the next line
      if (not file or not getline(file,line))
	throw myexception()<<"Missing partition after 'mini-branch'!";

      if (not line.size())
	throw myexception()<<"Empty line after 'mini-branch'!";

      double length = convertTo<double>(words[1]);
      Partition P(line);

      mini_branches.push_back(P);
      mini_branch_lengths.push_back(length);
    }
    else {
      if (with_lengths)
	throw myexception()<<"Don't understand line='"<<line<<"'";
      Partition P(line);
      branches.push_back(P);
      branch_lengths.push_back(1);
    }
  }

  if (log_verbose) cerr<<"draw-tree: Read "<<branches.size()<<" partitions"<<endl;

  //--------------- Construct MC tree with lengths  ---------------//
  branches = check_MC_partitions(branches);
  MC_tree_with_lengths MC(branches);

  for(int i=0;i<branches.size();i++)
  {
    double L = branch_lengths[i];

    int b1 = find_index(MC.partitions,branches[i]);
    if (b1 == -1)
      throw myexception()<<"Can't find partition I just added to tree!\n"<<"     "<<branches[i];

    int b2 = MC.reverse(b1);

    MC.branch_length(b1) = MC.branch_length(b2) = L;

    if (branches[i].full())
      MC.T.directed_branch(b1).set_length(L);
  }

  for(int i=0;i<nodes.size();i++)
  {
    double L = node_lengths[i];

    int b = MC.find_branch(nodes[i]);

    if (b == -1)
      throw myexception()<<"Can't find node in tree!\n"<<"    "<<nodes[i];

    int n = MC.mapping[b];
    int d = MC.degree(n);

    if (d < 4 and log_verbose) {
      cerr<<"draw-tree: Processing node '"<<nodes[i]<<"'"<<endl;
      cerr<<"draw-tree: Found as branch '"<<MC.partitions[b]<<"'"<<endl;
      cerr<<"draw-tree: degree = "<<MC.degree(n)<<endl;

      for(int i=0;i<2*MC.n_branches();i++)
	if (MC.mapping[i] == n)
	  cerr<<"draw-tree: "<<MC.partitions[i]<<endl;


      throw myexception()<<"Error: node length given for node of degree "<<d<<"!";
    }

    MC.node_length(n) = L;
  }

  return MC;
}

struct point_position
{
  double x;
  double y;
  double z;
  point_position():x(0),y(0),z(0) {}
  point_position(double x, double y):x(x),y(y),z(0) {}
  point_position(double x, double y, double z):x(x),y(y),z(z) {}
};

bool lines_cross(const point_position& m1,
		 const point_position& m2,
		 const point_position& n1,
		 const point_position& n2,
		 double& t,
		 double& s)
{
  double x1 = m1.x;
  double y1 = m1.y;

  double x2 = m2.x;
  double y2 = m2.y;

  double x3 = n1.x;
  double y3 = n1.y;

  double x4 = n2.x;
  double y4 = n2.y;

  //  ((A B)(C D)) * (t s) = (E F)
  // t is the m1-m2 coordinate
  // s is the n1-n2 coordinate

  double A = x2 - x1;
  double B = x3 - x4;
  double C = y2 - y1;
  double D = y3 - y4;
  double E = x3 - x1;
  double F = y3 - y1;


  double t_top = E*D-F*B;
  double s_top = A*F-E*C;
  double det = A*D-B*C;

  if (det == 0)
    return false;

  t = t_top/det;
  s = s_top/det;

  return true;
}

bool segments_cross(const point_position& m1,
		    const point_position& m2,
		    const point_position& n1,
		    const point_position& n2)
{
  double t,s;
  if (not lines_cross(m1,m2,n1,n2,t,s)) return false;

  bool cross = ((0 <= s) and (s <= 1) and (0 <= t) and (t <= 1));

  if (cross) {
    //    cerr<<"t = "<<t<<endl;
    //    cerr<<"s = "<<s<<endl;
  }
  return cross;
}

bool segments_cross_with_margin(const point_position& m1,
				const point_position& m2,
				const point_position& n1,
				const point_position& n2,
				double a,
				double b,
				double c,
				double d)
{
  double t,s;
  if (not lines_cross(m1,m2,n1,n2,t,s)) return false;

  bool cross = ((-a <= s) and (s <= 1+b) and (-c <= t) and (t <= 1+d));

  if (cross) {
    //    cerr<<"t = "<<t<<endl;
    //    cerr<<"s = "<<s<<endl;
  }
  return cross;
}

bool ray_points_to_segment(const point_position& m1,
			    const point_position& m2,
			    const point_position& n1,
			    const point_position& n2)
{
  double t,s;
  if (not lines_cross(m1,m2,n1,n2,t,s)) return false;

  bool cross = ((0 <= s) and (s <= 1) and (0 <= t));

  if (cross) {
    //    cerr<<"t = "<<t<<endl;
    //    cerr<<"s = "<<s<<endl;
  }
  return cross;
}

double distance_to_line_segment(const point_position& p1,
				const point_position& p2,
				const point_position& p3)
{
  double t = ((p1.x - p2.x)*(p3.x - p2.x) + (p1.y - p2.y)*(p3.y - p2.y))/
    ((p3.x - p2.x)*(p3.x - p2.x) + (p3.y - p2.y)*(p3.y - p2.y));

  if (t < 0) t = 0;
  if (t > 1) t = 1;

  double x = p2.x + t*(p3.x-p2.x);
  double y = p2.y + t*(p3.y-p2.y);

  return sqrt((p1.x-x)*(p1.x-x) + (p1.y-y)*(p1.y-y));
}


// FIXME - how to handle text layout?
//        (a) problem - we don't know the text sizes in the tree layout!
//        (b) we could try to find the page boundaries in the page drawing code.
//        (c) but then layout algorithms couldn't try to prevent label overlap.

// 1. FIXME - figure out how much angle space we REALLY have!
// 2. FIXME - give away unused angle space...
// 3. Place short branches in a cluster near the edge of our angle space,
//    place the longest ones in the middle!

// 4. HOW hard would it be to allow manual adjustment of the placements???

struct RGB 
{
  double R;
  double G;
  double B;
  RGB():R(0),G(0),B(0) {}
  RGB(double r, double g, double b):R(r),G(g),B(b) { }
};

struct common_layout
{
  vector<double> node_radius;
  vector<point_position> node_positions;
  vector<RGB> node_color;

  double xmin() const;
  double xmax() const;

  double ymin() const;
  double ymax() const;

  double x_width() const {return xmax() - xmin();}
  double y_width() const {return ymax() - ymin();}

  double x_center() const {return 0.5*(xmin()+xmax());}
  double y_center() const {return 0.5*(ymin()+ymax());}

  void center();

  void rotate(double alpha,double xc, double yc);

  void rotate_for_aspect_ratio(double xw,double yw);

  common_layout(int n)
    :node_radius(n,0),
     node_positions(n),
     node_color(n,RGB(1,1,1))
  {}
};

double common_layout::xmin() const
{
  double m = node_positions[0].x;
  for(int i=0;i<node_positions.size();i++)
    m = std::min(m,node_positions[i].x);
  return m;
}

double common_layout::xmax() const
{
  double m = node_positions[0].x;
  for(int i=0;i<node_positions.size();i++)
    m = std::max(m,node_positions[i].x);
  return m;
}

double common_layout::ymin() const
{
  double m = node_positions[0].y;
  for(int i=0;i<node_positions.size();i++)
    m = std::min(m,node_positions[i].y);
  return m;
}

double common_layout::ymax() const
{
  double m = node_positions[0].y;
  for(int i=0;i<node_positions.size();i++)
    m = std::max(m,node_positions[i].y);
  return m;
}



void common_layout::center()
{
  double xc = 0.5*(xmin() + xmax());
  double yc = 0.5*(ymin() + ymax());

  for(int i=0;i < node_positions.size();i++)
  {
    node_positions[i].x -= xc;
    node_positions[i].y -= yc;
  }
}


void common_layout::rotate(double alpha, double xc, double yc)
{
  for(int i=0;i < node_positions.size();i++)
  {
    double x = node_positions[i].x - xc;
    double y = node_positions[i].y - yc;

    node_positions[i].x = xc + cos(alpha)*x - sin(alpha)*y;
    node_positions[i].y = yc + sin(alpha)*x + cos(alpha)*y;
  }
}

void common_layout::rotate_for_aspect_ratio(double xw,double yw)
{
  double xc = x_center();
  double yc = y_center();
  const int n_parts = 100;
  double angle = 0;
  double scale = 0;
  for(int i=0;i<n_parts;i++) {
    double alpha = M_PI/n_parts * i;
    common_layout L = *this;
    L.rotate(alpha,xc,yc);
    double l_scale = std::min(xw/L.x_width(),yw/L.y_width());
    if (l_scale > scale) {
      angle = alpha;
      scale = l_scale;
    }
  }
  rotate(angle,xc,yc);
}



struct tree_layout: public common_layout
{
  SequenceTree T;

  void rotate_subtree(int b, double alpha);

  double edge_length_error() const;

  double edge_length_error(int) const;

  bool edges_cross() const;

  bool edges_cross(int b1, int b2) const;

  bool subtree_edges_cross(int b1, int b2) const;

  tree_layout(const SequenceTree& T1)
    :common_layout(T1.n_nodes()),
     T(T1)
  {}
};

double tree_layout::edge_length_error() const
{
  double error = 0;
  for(int b=0;b<T.n_branches();b++)
    error += edge_length_error(b);
  return error;
}

double tree_layout::edge_length_error(int b) const
{
  int n1 = T.directed_branch(b).source();
  int n2 = T.directed_branch(b).target();

  double L = T.directed_branch(b).length();
    
  double x1 = node_positions[n1].x;
  double y1 = node_positions[n1].y;
  
  double x2 = node_positions[n2].x;
  double y2 = node_positions[n2].y;
  
  double D2 = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
  double D  = sqrt(D2);
  return std::abs(log(L/D));
}

bool tree_layout::edges_cross(int e1, int e2) const
{
  int m1 = T.directed_branch(e1).source();
  int m2 = T.directed_branch(e1).target();

  int n1 = T.directed_branch(e2).source();
  int n2 = T.directed_branch(e2).target();

  if (m1 == n1 or m1 == n2) return false;
  if (m2 == n1 or m2 == n2) return false;

  return segments_cross(node_positions[m1],
			node_positions[m2],
			node_positions[n1],
			node_positions[n2]);
}

bool tree_layout::edges_cross() const
{
  for(int i=0;i<T.n_branches();i++)
    for(int j=0;j<i;j++)
      if (edges_cross(i,j))
	return true;

  return false;
}


bool tree_layout::subtree_edges_cross(int b1, int b2) const
{
  vector<const_branchview> branches1 = branches_after_inclusive(T, b1);
  vector<const_branchview> branches2 = branches_after_inclusive(T, b2);

  for(int i=0;i<branches1.size();i++)
    for(int j=0;j<branches2.size();j++)
      if (edges_cross(branches1[i],branches2[j]))
	return true;

  return false;
}


int find_directed_branch(const Tree& T,const Partition& p)
{
  for(int b=0; b<2*T.n_branches(); b++)
    if (branch_partition(T,b) == p.group2)
      return b;
  return -1;
}

int node_max_depth(const Tree& T,int node) 
{
  int depth = T.edges_distance(node,0);
  for(int i=1;i<T.n_leaves();i++) 
    depth = std::max(depth, T.edges_distance(node,i) );

  return depth;
}

int n_children(const Tree& T,int b)
{
  return branch_partition(T,b).count();
}


void equal_angle_layout(tree_layout& L,int parent,double min_a,double max_a)
{
  SequenceTree& T = L.T;

  if (T.directed_branch(parent).target().is_leaf_node()) return;

  double a1 = min_a;
  for(out_edges_iterator b = T.directed_branch(parent).branches_after();b;b++) 
  {
    double a2 = a1 + (max_a - min_a) * n_children(T,*b)/n_children(T,parent);
    double a = (a1+a2)/2.0;

    int n1 = (*b).source();
    int n2 = (*b).target();
    double length = (*b).length();

    L.node_positions[n2] = L.node_positions[n1];
    L.node_positions[n2].x += length*cos(a);
    L.node_positions[n2].y += length*sin(a);

    equal_angle_layout(L,*b,a1,a2);
    a1=a2;
  }
}

// Consider a Cayley tree that is fractally expanded
// iteratively, so that it has 3,9,21... internal edges.
// If n=1, the number of tips Tn=3*2^(n-1), the number of
// internal edges In = 3*(2^n - 1), the number of edges
// from tip-to-tip Dn = 2n.  We want Dn/In as a function
// of Tn (the degree), which gives us 
//
//   Dn/In  =  2n/3(2^n - 1)
//
//   Dn/In  = 2(log2(degree) - log2(3) + 1)/(2*degree - 3)

double node_ratio(int degree)
{
  assert(degree > 0);
  return (1.0 + log2(degree) - log2(3))/(2.0*degree - 3.0);
}


double node_diameter(double lengths,int degree)
{
  return lengths*node_ratio(degree)/node_ratio(4);
}


tree_layout equal_angle_layout(SequenceTree MF,vector<double> node_radius)
{
  /*

  vector<nodeview> nodes;
  for(int i=0;i<MF.n_nodes();i++)
    nodes.push_back(MF.node(i));

  vector<branchview> branches;

  for(int j=0;j<3;j++) {
    for(int i=0;i<MF.n_branches();i++)
      branches.push_back(MF.branch(i));

    for(int i=0;i<branches.size();i++) {
      
      nodeview n = MF.create_node_on_branch(branches[i]);
      
      out_edges_iterator temp = n.branches_out();
      branchview b1 = *temp;
      temp++;
      branchview b2 = *temp;
      
      double L = b1.length() + b2.length();
      b1.set_length(L/2.0);
      b2.set_length(L/2.0);
    }
  }

  // remap node radii
  vector<double> old_node_radius = node_radius;
  node_radius = vector<double>(MF.n_nodes(), 0.0);
  for(int i=0;i<old_node_radius.size();i++)
    node_radius[nodes[i]] = old_node_radius[i];
*/


  for(int b=0;b<MF.n_branches();b++) 
  {
    double L = MF.branch(b).length();
    int n1 = MF.branch(b).target();
    int n2 = MF.branch(b).source();
    L += node_radius[n1] + node_radius[n2];
    MF.branch(b).set_length(L);
  }

  // place the root
  int root = 0;
  for(int n=0;n<MF.n_nodes();n++)
    if (node_max_depth(MF,n) < node_max_depth(MF,root))
      root = n;
  
  // cerr<<"root = "<<root<<endl;

  tree_layout L(MF);
  L.node_radius = node_radius;

  double a1 = 0;
  for(out_edges_iterator b = MF.node(root).branches_out();b;b++) 
  {
    double a2 = a1 + 2.0 * M_PI * n_children(MF,*b)/MF.n_leaves();
    double a = (a1+a2)/2.0;
    
    // cerr<<"branch "<<(*b).name()<<"  n_children = "<<n_children(MF,*b)<<endl;

    int n1 = (*b).source();
    int n2 = (*b).target();
    double length = (*b).length();

    L.node_positions[n2] = L.node_positions[n1];
    L.node_positions[n2].x += length*cos(a);
    L.node_positions[n2].y += length*sin(a);

    // cerr<<"edge target = "<<n2<<"   a1 = "<<a1*180/M_PI<<" a = "<<a*360/(2*M_PI)<<" a2 = "<<a2*180/M_PI<<"   length = "<<length<<"  x = "<<    L.node_positions[n2].x<<" y = "<<L.node_positions[n2].x<<endl;
    equal_angle_layout(L,*b,a1,a2);
    a1 = a2;
  }
  return L;
}


tree_layout equal_angle_layout(const MC_tree_with_lengths& MC)
{
  SequenceTree MF = MC.T;

  // determine node lengths
  vector<double> node_radius(MF.n_nodes(),0);

  for(int n=0;n<MC.n_nodes();n++)
  {
    int b = MC.branch_to_node(n);
    if (not MC.partitions[b].full())
      throw myexception()<<"Hey!  This procedure only works for full partitions!";

    int mf_n = MF.directed_branch(b).target();
    int d = MF.node(mf_n).degree();
    node_radius[mf_n] = 0;

    // The degree could be zero is this node is at the end of a branch that
    // directly wanders over another branch.
    if (d > 0)
      node_radius[mf_n] = node_diameter(MC.node_length(n), d)/2.0;
  }

  return equal_angle_layout(MF,node_radius);
}

void rotate(point_position& pp,double xc, double yc, double alpha)
{
  pp.x -= xc;
  pp.y -= yc;
  
  double X = pp.x;
  double Y = pp.y;

  pp.x = cos(alpha)*X - sin(alpha)*Y;
  pp.y = sin(alpha)*X + cos(alpha)*Y;

  pp.x += xc;
  pp.y += yc;
}

void tree_layout::rotate_subtree(int b,double alpha)
{
  double xc = node_positions[T.directed_branch(b).source()].x;
  double yc = node_positions[T.directed_branch(b).source()].y;

  vector<const_branchview> branches = branches_after_inclusive(T, b);
  for(int i=0;i < branches.size();i++)
  {
    point_position& pp = node_positions[branches[i].target()];
    ::rotate(pp,xc,yc,alpha);
  }
}

double get_angle(const tree_layout& L, int b)
{
  const Tree& T = L.T;
  int source = T.directed_branch(b).source();
  int target = T.directed_branch(b).target();

  const point_position& s = L.node_positions[source];
  const point_position& t = L.node_positions[target];

  return atan2(t.y - s.y, t.x - s.x);
}

double normalize_angle(double a)
{
  a = fmod(a, 2*M_PI);
  if (a < 0)
    a += 2*M_PI;
  return a;
}


// x - y: should always be non-negative
double circular_minus(double x, double y)
{
  x = normalize_angle(x);
  y = normalize_angle(y);

  if (x >= y) 
    return x-y;
  else
    return (2.0*M_PI - (y-x));
}

// A (clockwise) circular range structure
// All entries should be between 0 and 2 PI... unless we are a full circle range
class circular_range 
{
  double min_;
  double max_;
  bool full_circle;
public:
  bool is_full_circle()  const {return full_circle;}

  void set_full_circle()  {
    full_circle = true;
    min_ = 0;
    max_ = 0;
  }

  double min() const {
    if (full_circle) std::abort();
    assert(min_ >=0 and min_ <= 2*M_PI);
    return min_;
  }

  double max() const {
    if (full_circle) std::abort();
    assert(max_ >=0 and max_ <= 2*M_PI);
    return max_;
  }

  void min(double x) 
  {
    if (full_circle) std::abort();
    min_ = normalize_angle(x); 
  }

  void max(double x) 
  {
    if (full_circle) std::abort();
    max_ = normalize_angle(x); 
  }

  circular_range():min_(-1),max_(-1),full_circle(false) {}

  circular_range(double d1,double d2) 
    :full_circle(false)
  {
    min(d1);
    max(d2);
  }

  double measure() 
  {
    if (full_circle)
      return 2*M_PI;
    else
      return circular_minus(max_,min_);
  }

  circular_range& shift(double d) {
    if (not full_circle) {
      min(min()+d);
      max(max()+d);
    }
    return *this;
  }

  bool contains(double d) const 
  {
    if (full_circle) return true;

    assert(min_ >=0 and min_ <= 2*M_PI);
    assert(max_ >=0 and max_ <= 2*M_PI);

    d = normalize_angle(d);
    if (min() <= max()) 
      return ((min() <= d) and (d <= max()));
    else
      return ((d <= max()) or (min() <= d));
  }

  const circular_range& operator+=(circular_range& r);
  
  double operator-(const circular_range& r) const;
};

ostream& operator<<(ostream& o, const circular_range& R)
{
  if (R.is_full_circle())
    o<<"(full_circle)";
  else
    o<<"("<<R.min()<<", "<<R.max()<<")";
  return o;
}

bool overlaps(const circular_range& r1, circular_range& r2)
{
  if (r1.is_full_circle() or r2.is_full_circle()) return true;
  if (r1.contains(r2.min()) or r1.contains(r2.max())) return true;
  if (r2.contains(r1.max()) or r2.contains(r1.max())) return true;
  return false;
}

circular_range full_circle_range()
{
  circular_range R;
  R.set_full_circle();
  return R;
}

double circular_range::operator-(const circular_range& r) const
{
  if (is_full_circle() or r.is_full_circle() or contains(r.max())) return 0.0;

  return circular_minus(min(),r.max());
}

// probably this segment should NOT contain the origin
circular_range circular_range_from_line_segment(double x1, double y1, double x2, double y2)
{
  double xm = 0.5*(x1+x2);
  double ym = 0.5*(y1+y2);

  double A1 = atan2(y1,x1);
  double Am = atan2(ym,xm);
  double A2 = atan2(y2,x2);

  bool O1 = (x1 == 0 and y1 == 0);
  bool O2 = (x2 == 0 and y2 == 0);
  if (O1 and not O2) return circular_range(A2,A2);
  if (O2 and not O1) return circular_range(A1,A1);

  if (distance_to_line_segment(point_position(0,0),point_position(x1,y1),point_position(x2,y2)) == 0)
    throw myexception()<<"line segments through the origin do not give meaningful ranges.";

  circular_range R1(A1,A2);
  circular_range R2(A2,A1);

  if (R1.contains(Am))
    return R1;
  else
    return R2;
}

const circular_range& circular_range::operator+=(circular_range& r)
{
  // first handle the case where we have a full circle
  if (r.is_full_circle())
    set_full_circle();
  if (full_circle) return *this;

  //  cout<<"operator+=:"<<endl;
  //  cout<<(*this)<<endl;
  //  cout<<r<<endl;

  double M = measure();
  double rmax = circular_minus(r.max(), min());
  double rmin = circular_minus(r.min(), min());
  //  cout<<"M = "<<M<<endl;

  if (rmin <= M) {
    if (rmax < rmin)
      set_full_circle();
    else if (rmax <= M)
      ;
    else
      max(r.max());
  }
  else if (rmax <= M)
  {
    min(r.min());
  }
  else {
    if (rmax < rmin)
      *this = r;
    else if (rmin - M < 2*M_PI - rmax)
      max(r.max());
    else
      min(r.min());
  }

#ifndef NDEBUG
  double M2 = measure();
  assert( M2 >= M);
#endif
  
  return *this;;
}

  
circular_range get_angles(const tree_layout& L,int b,int n)
{
  const Tree& T = L.T;
  const point_position& c = L.node_positions[n];

  double A = get_angle(L,b);
  circular_range R(A,A);

  vector<const_branchview> branches = branches_after_inclusive(L.T,b);
  // skip branch b - only consider its children
  // .. because of the difficulty of handling line segments from the origin
  //    (This seems to work... for now.)
  for(int i=1;i<branches.size();i++) 
  {
    int source = T.directed_branch(branches[i]).source();
    int target = T.directed_branch(branches[i]).target();
    
    const point_position& s = L.node_positions[source];
    const point_position& t = L.node_positions[target];

    circular_range A2 = circular_range_from_line_segment(s.x - c.x, s.y - c.y, t.x - c.x, t.y - c.y);

    // FIXME - we should really determine the angle range of the line,
    //         instead of just the angle of the endpoint.
    R += A2;
  }

  return R;
}


// check if any nodes do not have positive daylight between subtrees
bool subtrees_of_node_overlap(tree_layout& L,int n)
{
  const Tree& T = L.T;

  // get outwards branches - which point to subtrees
  vector<const_branchview> branches;
  for(const_out_edges_iterator i = T.node(n).branches_out();i;i++)
    branches.push_back(*i);
  
  vector<circular_range> angles;
  for(int i=0;i<branches.size();i++)  {
    angles.push_back( get_angles(L,branches[i],n) );
    for(int j=0;j<i;j++)
      if (overlaps(angles[j],angles[i]))
	return true;
  }

  return false;
}

// check if any nodes do not have positive daylight between subtrees
bool shades(tree_layout& L)
{
  for(int i=0;i<L.T.n_nodes();i++)
    if (subtrees_of_node_overlap(L,i))
      return true;

  return false;
}

void equalize_daylight(tree_layout& L,int n)
{
  const Tree& T = L.T;

  // get outwards branches - which point to subtrees
  vector<int> branches;
  for(const_out_edges_iterator i = T.node(n).branches_out();i;i++)
    branches.push_back(*i);
  

  //get the circular ranges for branch subtree
  vector<circular_range> angles;
  for(int i=0;i<branches.size();i++) 
    angles.push_back( get_angles(L,branches[i],n) );

  for(int b=0;b<branches.size();b++)
  {
    vector<double> daylight_before(branches.size(), 2.0*M_PI);
    vector<double> daylight_after(branches.size(), 2.0*M_PI);

    for(int i=0;i<branches.size();i++) {
      for(int j=0;j<branches.size();j++) 
	if (i!=j) {
	  double delta = angles[j] - angles[i];
	  daylight_after[i] = std::min(daylight_after[i],delta);
	  daylight_before[j] = std::min(daylight_before[j],delta);
	}
    }

    if (daylight_after[b] > 0 and daylight_before[b] > 0)
    {
      double delta = (daylight_after[b] - daylight_before[b])/2.0;
      angles[b].shift(delta);
      L.rotate_subtree(branches[b],delta);
    }
  }
}

void equalize_daylight_greedy(tree_layout& L,int n)
{
  const Tree& T = L.T;

  // get outwards branches - which point to subtrees
  vector<const_branchview> branches;
  for(const_out_edges_iterator i = T.node(n).branches_out();i;i++) {
    branches.push_back(*i);
    if ((*i).is_leaf_branch()) {
      cout<<"";
    }
  }
  

  //get the circular ranges for branch subtree
  vector<circular_range> angles;
  double total_occlusion = 0;
  for(int i=0;i<branches.size();i++)  {
    angles.push_back( get_angles(L,branches[i],n) );
    total_occlusion += angles.back().measure();
  }

  if (total_occlusion < 2*M_PI)
  {
    double daylight_per_subtree = (2*M_PI - total_occlusion)/branches.size();

    for(int b=1; b<branches.size(); b++) 
    {
      double theta = circular_minus(angles[b].min(), angles[b-1].max());
      double delta = daylight_per_subtree - theta;

      if (delta != 0) {
	L.rotate_subtree(branches[b], delta);
	angles[b].shift(delta);
      }
    }
  }

  if (L.edges_cross()) throw myexception()<<"equalize_daylight(L,n): edges cross - should not happen!";
}

// the size of a split is the size of its smallest set.
// the size of a node (a multi-partition) is the size of its largest partition.
// this will be the size of the second-largest set.
int node_size(const Tree& T,int n)
{
  if (n < 2) std::abort();

  vector<int> S;
  for(const_out_edges_iterator i = T.node(n).branches_out();i;i++)
    S.push_back(n_children(T,*i));

  std::sort(S.begin(),S.end());

  return S[S.size()-2];
}

vector<int> node_order(const Tree& T)
{
  vector<int> nodes;
  vector<int> S;
  for(int i=T.n_leaves();i<T.n_nodes();i++) 
  {
    nodes.push_back(i);
    S.push_back(node_size(T,nodes.back()));
  }

  std::sort(nodes.begin(), nodes.end(), sequence_order<int>(S));
  std::reverse(nodes.begin(), nodes.end());

  return nodes;
}

void equalize_daylight(tree_layout& L, bool greedy, bool no_shade)
{
  const Tree& T = L.T;
  vector<int> nodes = node_order(T);

  for(int i=0;i<nodes.size();i++) {
    tree_layout L2 = L;
    if (greedy)
      equalize_daylight_greedy(L2,nodes[i]);
    else
      equalize_daylight(L2,nodes[i]);
    if (not no_shade or not shades(L2))
      L = L2;
  }
}

tree_layout equal_daylight_layout(SequenceTree MF, const vector<double>& node_radius,
				  bool greedy, bool no_shade)
{
  tree_layout L = equal_angle_layout(MF,node_radius);

  for(int i=0;i<3;i++)
    equalize_daylight(L, greedy, no_shade);

  return L;
}

tree_layout equal_daylight_layout(const MC_tree_with_lengths& MC,  bool greedy=false, bool no_shade=false)
{
  SequenceTree MF = MC.T;

  // determine node lengths
  vector<double> node_radius(MF.n_nodes(),0);

  for(int n=0;n<MC.n_nodes();n++)
  {
    int b = MC.branch_to_node(n);
    if (not MC.partitions[b].full())
      throw myexception()<<"Hey!  This procedure only works for full partitions!";

    int mf_n = MF.directed_branch(b).target();
    int d = MF.node(mf_n).degree();
    node_radius[mf_n] = 0;

    // The degree could be zero is this node is at the end of a branch that
    // directly wanders over another branch.
    if (d > 0)
      node_radius[mf_n] = node_diameter(MC.node_length(n), d)/2.0;
  }

  return equal_daylight_layout(MF,node_radius,greedy,no_shade);
}

vector<int> walk_tree(const tree_layout& TL, const vector<int>& edges)
{
  vector<int> edges_order;

  int e = edges[0];
  
  do {

    edges_order.push_back(e);

    double A0 = get_angle(TL,e);

    vector<const_branchview> children;
    append(TL.T.directed_branch(e).branches_after(),children);

    if (children.size()) {
      vector<double> angles(children.size());
      for(int i=0;i<children.size();i++)
	angles[i] = circular_minus(get_angle(TL,children[i]), A0);

      int nexti = argmin(angles);
      e = children[nexti];
    }
    else 
      e = TL.T.directed_branch(e).reverse();

    
  } while (e != edges[0]);

  return edges_order;
}

vector<int> walk_tree(const tree_layout& TL)
{
  vector<int> edges;
  for(int i=0;i<TL.T.n_branches();i++)
    edges.push_back(i);

  return walk_tree(TL,edges);
}



double get_text_length(cairo_t* cr, const string& s)
{
  cairo_text_extents_t extents;
  cairo_text_extents (cr, s.c_str(), &extents);
  return extents.width;
}

double get_text_height(cairo_t* cr, const string& s)
{
  cairo_text_extents_t extents;
  cairo_text_extents (cr, s.c_str(), &extents);
  return extents.height;
}

struct tree_plotter: public cairo_plotter
{
  bool horizontal_names;
  tree_layout L;
  void operator()(cairo_t*);
  tree_plotter(const tree_layout& tl,double xw, double yw)
    :cairo_plotter(xw,yw),
     horizontal_names(true),
     L(tl) 
  {}
  tree_plotter(const tree_layout& tl,double xw, double yw,double fs)
    :cairo_plotter(xw,yw,fs),
     horizontal_names(true),
     L(tl) 
  {}
};

void tree_plotter::operator()(cairo_t* cr)
{
  double xc = 0.5*(L.xmin() + L.xmax());
  double yc = 0.5*(L.ymin() + L.ymax());
  double xw = L.x_width();
  double yw = L.y_width();

  // move to center and flip up 
  cairo_translate(cr, page_x_width()*inch/2.0, page_y_width()*inch/2.0);

  // find scaling factor
  const double edge = 0.10;
  const double factor = 1.0-edge*2;

  double scale = std::min(page_x_width()*inch/xw,page_y_width()*inch/yw);
  scale *= factor;
  cairo_scale(cr, scale, scale);
  cairo_translate(cr, -xc, -yc);

  const double pt = 1.0/scale;
  const double line_width = 2.0*pt;
  const double dashes[] = {3.0*line_width, 3.0*line_width};
  cairo_set_line_width(cr, line_width);

  // draw the branches
  for(int b=0;b<L.T.n_branches();b++) 
  {
    int n1 = L.T.branch(b).source();
    int n2 = L.T.branch(b).target();

    double x1 = L.node_positions[n1].x;
    double y1 = L.node_positions[n1].y;

    double x2 = L.node_positions[n2].x;
    double y2 = L.node_positions[n2].y;

    cairo_move_to (cr, x1, y1);
    cairo_line_to (cr, x2, y2);
    cairo_set_source_rgb (cr, 0, 0 ,0);
    cairo_stroke (cr);
  }

  cairo_set_dash (cr, dashes, 2, 0.0);

  // draw the circles
  for(int n=0;n<L.node_radius.size();n++) 
  {
    double x = L.node_positions[n].x;
    double y = L.node_positions[n].y;

    if (L.node_radius[n] > 0) {
      cairo_save(cr);
      RGB& color = L.node_color[n];
      // cerr<<"node n="<<n<<"  radius = "<<L.node_radius[n]<<endl;
      cairo_arc(cr, x, y, L.node_radius[n], 0.0, 2.0 * M_PI);

      cairo_set_source_rgb (cr, color.R , color.G, color.B);
      cairo_fill_preserve (cr);

      cairo_set_source_rgb (cr, 0 , 0, 0);
      cairo_set_line_width(cr, line_width/2.0);
      cairo_stroke(cr);
      cairo_restore(cr);
    }
  }

  cairo_select_font_face (cr, "Sans", 
			  CAIRO_FONT_SLANT_NORMAL,
			  CAIRO_FONT_WEIGHT_BOLD);
  cairo_set_font_size (cr, font_size()/scale);
  
  if (horizontal_names)
    for(int l=0;l<L.T.n_leaves();l++)
    { 
      double x1 = L.node_positions[l].x;
      double y1 = L.node_positions[l].y;

      int p = L.T.branch(l).target();
      
      double x2 = L.node_positions[p].x;
      double y2 = L.node_positions[p].y;
      
      double a = atan2(y1-y2,x1-x2);
      
      double W = get_text_length(cr, L.T.get_label(l));
      double H = get_text_height(cr, L.T.get_label(l));
      
      x1 += cos(a)*(line_width+H/8);
      y1 += sin(a)*(line_width+H/2);
      
      y1 += 0.5*H;
      
      if (cos(a) < 0) x1 -= W;
      
      cairo_move_to (cr, x1, y1);
      cairo_show_text (cr, L.T.get_label(l).c_str());
    }
  else
    for(int l=0;l<L.T.n_leaves();l++)
    {  
      double x2 = L.node_positions[l].x;
      double y2 = L.node_positions[l].y;

      int p = L.T.branch(l).target();

      double x1 = L.node_positions[p].x;
      double y1 = L.node_positions[p].y;

      double dx = x2 - x1;
      double dy = y2 - y1;

      double a = atan2(dy,dx);

      double W = get_text_length(cr, L.T.get_label(l));
      double H = get_text_height(cr, L.T.get_label(l));

      //    cout<<L.T.get_label(l).c_str()<<"   dx = "<<dx<<"  dy = "<<dy<<"   a = "<<a<<endl;

      cairo_save(cr);
      {
	cairo_translate (cr, x2, y2);
	cairo_move_to(cr, 0, 0);
	cairo_rotate(cr, a);
	if (cos(a)>=0)
	{
	  cairo_move_to(cr, H/8, H*0.5);
	  cairo_show_text (cr, L.T.get_label(l).c_str());
	}
	else
        {
	  cairo_move_to(cr, H/4+W, -H*0.5);

	  // cairo_translate(cr, W, 0);
	  cairo_scale(cr, -1, -1);
	  cairo_show_text (cr, L.T.get_label(l).c_str());
	}
      }
      cairo_restore(cr);
    }
}

struct graph_layout: public common_layout
{
  MC_tree_with_lengths MC;

  bool edges_cross(int b1,int b2) const;

  bool edges_cross() const;

  double edge_length_error() const;

  double edge_length_error(int) const;

  graph_layout(const MC_tree_with_lengths& T)
    :common_layout(T.n_nodes()),
     MC(T)
  {
    // determine node radii
    for(int n=0;n<MC.n_nodes();n++) {
      node_radius[n] = 0;
      int d = MC.degree(n);

      // The degree could be zero is this node is at the end of a branch that
      // directly wanders over another branch.
      if (d > 0)
	node_radius[n] = node_diameter(MC.node_length(n), d)/2.0;
    }
  }
};

double graph_layout::edge_length_error() const
{
  double error = 0;
  for(int e=0;e<MC.edges.size();e++) 
    error += edge_length_error(e);
  return error;
}

double graph_layout::edge_length_error(int e) const
{
  int n1 = MC.edges[e].from;
  int n2 = MC.edges[e].to;
  int t = MC.edges[e].type;

  double L = 0;
  if (t == 1) {
    int b = MC.edges[e].partition;
    L = MC.branch_length(b);
    
    double x1 = node_positions[n1].x;
    double y1 = node_positions[n1].y;
    
    double x2 = node_positions[n2].x;
    double y2 = node_positions[n2].y;
    
    double D2 = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
    double D  = sqrt(D2);
    return std::abs(log(L/D));
  }
  else
    return 0;
}

bool graph_layout::edges_cross(int e1,int e2) const
{
  int m1 = MC.edges[e1].from;
  int m2 = MC.edges[e1].to;

  int n1 = MC.edges[e2].from;
  int n2 = MC.edges[e2].to;

  if (m1 == n1 or m1 == n2) return false;
  if (m2 == n1 or m2 == n2) return false;

  return segments_cross(node_positions[m1],
			node_positions[m2],
			node_positions[n1],
			node_positions[n2]);
}


bool graph_layout::edges_cross() const
{
  for(int i=0;i<MC.edges.size();i++)
    for(int j=0;j<i;j++)
      if (edges_cross(i,j))
	return true;
  return false;
}

graph_layout layout_on_circle(MC_tree_with_lengths& MC,double R)
{
  graph_layout GL(MC);

  vector<int> pi = random_permutation(MC.n_nodes());

  for(int i=0;i<MC.n_nodes();i++)
  {
    double angle = pi[i]*(2.0*M_PI/MC.n_nodes());
    GL.node_positions[i].x = R*cos(angle);
    GL.node_positions[i].y = R*sin(angle);
  }
  return GL;
}

double distance2(const graph_layout& GL,int n1,int n2)
{
  double x1 = GL.node_positions[n1].x;
  double y1 = GL.node_positions[n1].y;

  double x2 = GL.node_positions[n2].x;
  double y2 = GL.node_positions[n2].y;

  double dx = x2-x1;
  double dy = y2-y1;

  return dx*dx + dy*dy;
}

double distance(const graph_layout& GL,int n1,int n2)
{
  return sqrt(distance2(GL,n1,n2));
}

struct graph_energy_function
{
  virtual double operator()(const graph_layout& GL) const=0;
  virtual double operator()(const graph_layout& GL, vector<point_position>&) const=0;

  double node_node_repulsion(const graph_layout& GL,vector<point_position>& D,int n1,int n2, double C) const;
  double node_node_attraction(const graph_layout& GL,vector<point_position>& D, int n1,int n2, double C, double l) const;
  double node_edge_attraction(const graph_layout& GL,vector<point_position>& D, int n1,int n2,int n3, double C) const;

  virtual ~graph_energy_function() {}
};

// In fr, (attractive) scaling factor is d*d/k, where k=sqrt(w*h/num_vertices)
// In fr, (repulsive) scaling factor is k*k/d, where d=distance

double graph_energy_function::node_node_repulsion(const graph_layout& GL, vector<point_position>& DEL, int n1, int n2, double C) const
{
  const int p=1;

  assert(n1 != n2);

  // don't repel the ends of the branch that I'm attached to
  if (GL.MC.connected(n1,n2) == 1) return 0;
	
  // don't repel nodes that wander over me, or that I wander to
  if (GL.MC.connected(n1,n2) == 2 or GL.MC.connected(n2,n1) == 2) return 0;

  C /= GL.MC.n_nodes();

  // FIXME - also don't repel other nodes that wander over same branch.

  double x1 = GL.node_positions[n1].x;
  double y1 = GL.node_positions[n1].y;

  double x2 = GL.node_positions[n2].x;
  double y2 = GL.node_positions[n2].y;

  double dx = x2 - x1;
  double dy = y2 - y1;

  double D = sqrt(dx*dx + dy*dy);

  double dL = abs(D - GL.node_radius[n1] - GL.node_radius[n2]);

  if (dL < 1.0e-15) dL=1.0e-15;
  double E = C/pow(dL,p);

  if (D < 1.0e-15) D=1.0e-15;
  double temp = C/(pow(dL,p+1)*D);

  DEL[n1].x += dx*temp;
  DEL[n1].y += dy*temp;

  DEL[n2].x -= dx*temp;
  DEL[n2].y -= dy*temp;

  return E;
}

double graph_energy_function::node_node_attraction(const graph_layout& GL, vector<point_position>& DEL, int n1, int n2, double C, double l) const
{
  assert(n1 != n2);

  double x1 = GL.node_positions[n1].x;
  double y1 = GL.node_positions[n1].y;

  double x2 = GL.node_positions[n2].x;
  double y2 = GL.node_positions[n2].y;

  double D2 = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
  double D  = sqrt(D2);
  double dL = D - GL.node_radius[n1] - GL.node_radius[n2] - l;

  double E = C*dL*dL;

  if (D < 1.0e-15) D=1.0e-15;
  double temp = 2.0*C*dL/D;

  DEL[n1].x += (x1-x2)*temp;
  DEL[n1].y += (y1-y2)*temp;

  DEL[n2].x += (x2-x1)*temp;
  DEL[n2].y += (y2-y1)*temp;

  assert(not isnan(E));
  return E;
}

double trunc(double t,double D)
{
  if (t <= 0 or t >= 1)
    D = 0;
  return D;
}

double distance_to_line_segment(const graph_layout& GL,int n1,int n2,int n3)
{
  return distance_to_line_segment(GL.node_positions[n1],
				  GL.node_positions[n2],
				  GL.node_positions[n3]);
}



// E = C*D(y1,y2)^2
// y1 = x1
// y2 = x2 + t*(x3-x2)
//  t = ...

//FIXME - add target distance (currently assumed to be 0)?

double graph_energy_function::node_edge_attraction(const graph_layout& GL, vector<point_position>& DEL, int n1, int n2,int n3, double C) const
{
  const point_position& p1 = GL.node_positions[n1];
  const point_position& p2 = GL.node_positions[n2];
  const point_position& p3 = GL.node_positions[n3];

  double x11 = p1.x;
  double x12 = p1.y;

  double x21 = p2.x;
  double x22 = p2.y;

  double x31 = p3.x;
  double x32 = p3.y;

  double t_high = ((x11 - x21)*(x31 - x21) + (x12 - x22)*(x32 - x22));
  double t_low  = ((x31 - x21)*(x31 - x21) + (x32 - x22)*(x32 - x22));

  double t = t_high/t_low;

  if (t < 0) t = 0;
  if (t > 1) t = 1;

  double y11 = x11;
  double y12 = x12;

  double y21 = x21 + t*(x31 - x21);
  double y22 = x22 + t*(x32 - x22);

  double D =  sqrt((y11-y21)*(y11-y21) + (y12-y22)*(y12-y22));

  double E = C*D*D;

  double dE_dD = C*2.0*D;

  double dD_dy11 = (y11 - y21)/D;
  double dD_dy12 = (y12 - y22)/D;

  double dD_dy21 = - dD_dy11;
  double dD_dy22 = - dD_dy12;

  //  cerr<<" D = "<<D<<" D2 = "<<distance_to_line_segment(GL,n1,n2,n3)<<" E = "<<E<<"  t = "<<t<<"   dE_dD = "<<dE_dD<<endl;

  double dt_dx11 = trunc(t, (x31-x21) / t_low );
  double dt_dx12 = trunc(t, (x32-x22) / t_low );

  double dt_dx21 = trunc(t, (t_low * (2.0*x21 - x11 - x31) - t_high * 2.0* (x21-x31))/(t_low*t_low) );
  double dt_dx22 = trunc(t, (t_low * (2.0*x22 - x12 - x32) - t_high * 2.0* (x22-x32))/(t_low*t_low) );

  double dt_dx31 = trunc(t, (t_low * (x11 - x21)           - t_high * 2.0* (x31-x21))/(t_low*t_low) );
  double dt_dx32 = trunc(t, (t_low * (x12 - x22)           - t_high * 2.0* (x32-x22))/(t_low*t_low) );



  double dy21_dx11 =         (x31-x21)*dt_dx11;
  double dy21_dx12 =         (x31-x21)*dt_dx12;

  double dy22_dx11 =         (x32-x22)*dt_dx11;
  double dy22_dx12 =         (x32-x22)*dt_dx12;

  DEL[n1].x += dE_dD*(dD_dy11 + dD_dy21*dy21_dx11 + dD_dy22*dy22_dx11);
  DEL[n1].y += dE_dD*(dD_dy12 + dD_dy21*dy21_dx12 + dD_dy22*dy22_dx12);

  double dy21_dx21 = 1.0 - t + (x31-x21)*dt_dx21;
  double dy21_dx22 =           (x31-x21)*dt_dx22;

  double dy22_dx21 =           (x32-x22)*dt_dx21;
  double dy22_dx22 = 1.0 - t + (x32-x22)*dt_dx22;

  DEL[n2].x += dE_dD*(          dD_dy21*dy21_dx21 + dD_dy22*dy22_dx21);
  DEL[n2].y += dE_dD*(          dD_dy21*dy21_dx22 + dD_dy22*dy22_dx22);

  double dy21_dx31 =       t + (x31-x21)*dt_dx31;
  double dy21_dx32 =           (x31-x21)*dt_dx32;

  double dy22_dx31 =           (x32-x22)*dt_dx31;
  double dy22_dx32 =       t + (x32-x22)*dt_dx32;

  DEL[n3].x += dE_dD*(          dD_dy21*dy21_dx31 + dD_dy22*dy22_dx31);
  DEL[n3].y += dE_dD*(          dD_dy21*dy21_dx32 + dD_dy22*dy22_dx32);

  return E;
}


double angle_difference(double a1, double a2)
{
  a1 = wrap(a1,-M_PI,M_PI);
  a2 = wrap(a2,-M_PI,M_PI);

  if (a1 > a2) std::swap(a1,a2);

  return std::min (a2-a1,a1+M_PI*2-a2);
}

double get_angle_derivative(double x11, double x12,
			    double x21, double x22, 
			    double x31, double x32,
			    double& da_dx11, double& da_dx12, 
			    double& da_dx21, double& da_dx22,
			    double& da_dx31, double& da_dx32)
{
  double A = (x11*x11 + x12*x12) - 2*(x11*x31+x12*x32) + (x31*x31+x32*x32);
  double B = (x21*x21 + x22*x22) - 2*(x21*x31+x22*x32) + (x31*x31+x32*x32);
  //  cerr<<"  A = "<<A<<" B = "<<B<<endl;
  //  cerr<<"  x21 = "<<x21<<"    x22 = "<<x22<<endl;
  double M = A*B;
  double H = (x11*x21+x12*x22) - (x11*x31+x12*x32) - (x21*x31+x22*x32) + (x31*x31+x32*x32);
  double L = sqrt(M);
  double D = H/L;

  //  cerr<<"D = "<<D<<"  L = "<<L<<"H = "<<H<<"  A = "<<A<<" B = "<<B<<endl;
  assert(not isnan(x21) and std::isfinite(x21));
  assert(not isnan(x22) and std::isfinite(x22));
  assert(not isnan(D) and std::isfinite(D));
  D = std::max(D,-1.0);
  D = std::min(D,1.0);
  double a = acos(D);
  assert(not isnan(a) and std::isfinite(a));

  double dA_dx11 = 2*(x11 - x31), dA_dx12 = 2*(x12 - x32);
  double dA_dx21 = 0            , dA_dx22 = 0;
  double dA_dx31 = 2*(x31 - x11), dA_dx32 = 2*(x32 - x12);

  //----------------------------------------------------//

  double dB_dx11 = 0            , dB_dx12 = 0;
  double dB_dx21 = 2*(x21 - x31), dB_dx22 = 2*(x22 - x32);
  double dB_dx31 = 2*(x31 - x21), dB_dx32 = 2*(x32 - x22);

  //----------------------------------------------------//

  double dH_dx11 = x21 - x31, dH_dx12 = x22 - x32;
  double dH_dx21 = x11 - x31, dH_dx22 = x12 - x32;
  double dH_dx31 = -x11 - x21 + 2*x31, dH_dx32 = -x12 - x22 + 2*x32;

  //----------------------------------------------------//

  double dL_dx11 = 0.5/L*(dA_dx11*B + dB_dx11*A);
  double dL_dx12 = 0.5/L*(dA_dx12*B + dB_dx12*A);
  
  double dL_dx21 = 0.5/L*(dA_dx21*B + dB_dx21*A);
  double dL_dx22 = 0.5/L*(dA_dx22*B + dB_dx22*A);
  
  double dL_dx31 = 0.5/L*(dA_dx31*B + dB_dx31*A);
  double dL_dx32 = 0.5/L*(dA_dx32*B + dB_dx32*A);

  //----------------------------------------------------//

  // FIXME: we can get SIGFPE when D==1
  if (std::abs(D-1) < 1-0e-8) D=1.0-1.0e-8;

  da_dx11 = -1.0/sqrt(1-D*D) * (L*dH_dx11 - H*dL_dx11)/M;
  da_dx12 = -1.0/sqrt(1-D*D) * (L*dH_dx12 - H*dL_dx12)/M;

  da_dx21 = -1.0/sqrt(1-D*D) * (L*dH_dx21 - H*dL_dx21)/M;
  da_dx22 = -1.0/sqrt(1-D*D) * (L*dH_dx22 - H*dL_dx22)/M;

  da_dx31 = -1.0/sqrt(1-D*D) * (L*dH_dx31 - H*dL_dx31)/M;
  da_dx32 = -1.0/sqrt(1-D*D) * (L*dH_dx32 - H*dL_dx32)/M;

  return a;
}


// cot(a/2) + pi^2 - a^2
double angle_penalty(double a)
{
  assert(-M_PI <= a and a <= M_PI);
  assert(a >= 0);
  a = abs(a);
  return 1.0/tan(a/2) + M_PI*M_PI;// - a*a;
}

double angle_deriv(double a)
{
  assert(-M_PI <= a and a <= M_PI);
  assert(a >= 0);
  a = abs(a);
  double s = sin(a/2.0);
  return -0.5/(s*s);// - 2*a;
}



double angle_energy(const graph_layout& GL,vector<point_position>& D,
		    int n1,int n2,int n3,double theta,
		    double (*angle_penalty)(double),
		    double(*angle_deriv)(double),
		    double C)
{
  const point_position& p1 = GL.node_positions[n1];
  const point_position& p2 = GL.node_positions[n2];
  const point_position& p3 = GL.node_positions[n3];

  double x11 = p1.x;
  double x12 = p1.y;
  
  double x21 = p2.x;
  double x22 = p2.y;
  
  double x31 = p3.x;
  double x32 = p3.y;

  double da_dx11,da_dx12,da_dx21,da_dx22,da_dx31,da_dx32;

  double a = get_angle_derivative(x11,x12,x21,x22,x31,x32,
				  da_dx11,da_dx12,
				  da_dx21,da_dx22,
				  da_dx31,da_dx32);

  //  cerr<<"a = "<<a<<"   isnan(a) = "<<isnan(a)<<"   isnormal(a) = "<<isnormal(a)<<endl;
  assert(std::isnormal(a));
  if (not std::isnormal(a))
    std::abort();
  assert(abs(a-theta) < 1.0e-5);

  double E = C*angle_penalty(a);
  double DE = C*angle_deriv(a);

  D[n1].x += DE*da_dx11;
  D[n1].y += DE*da_dx12;

  D[n2].x += DE*da_dx21;
  D[n2].y += DE*da_dx22;

  D[n3].x += DE*da_dx31;
  D[n3].y += DE*da_dx32;

  return E;
}


vector<int> nodes_after(const MC_tree& MC,int b1)
{
  vector<int> mask(MC.n_nodes(),0);
  mask[MC.mapping[b1]] = 1;
  for(int b2=0;b2 < 2*MC.n_branches();b2++)
    if (MC.left_of(b1,b2) or MC.wanders_over(b1,b2)) {
      int b3 = MC.reverse(b2);
      mask[MC.mapping[b2]] = 1;
      mask[MC.mapping[b3]] = 1;
    }

  vector<int> nodes;
  for(int i=0;i<MC.n_nodes();i++)
    if (mask[i])
      nodes.push_back(i);

  return nodes;
}

ublas::matrix<int> edge_crossing_matrix(const graph_layout& GL)
{
  const unsigned n_edges = GL.MC.edges.size();

  //--- check all edge pairs to see if they cross ---//
  ublas::matrix<int> e_cross(n_edges,n_edges);
  for(int i=0;i<e_cross.size1();i++)
    for(int j=0;j<e_cross.size2();j++)
      if (i==j)
	e_cross(i,j) = 0;
      else
	e_cross(i,j) = GL.edges_cross(i,j);

  return e_cross;
}

vector<bool> edge_crossing_vector(const graph_layout& GL)
{
  ublas::matrix<int> e_cross = edge_crossing_matrix(GL);

  vector<bool> e_cross_v(e_cross.size1(),false);

  for(int i=0;i < e_cross.size1(); i++)
    for(int j=0;j < e_cross.size2(); j++)
      if (e_cross(i,j) and GL.MC.edges[i].type == 1 and GL.MC.edges[j].type == 1 ) {
	e_cross_v[i] = true;
	e_cross_v[j] = true;
      }

  return e_cross_v;  
}

ublas::matrix<int> crossed_nodes_matrix(const graph_layout& GL)
{
  ublas::matrix<int> e_cross = edge_crossing_matrix(GL);

  const MC_tree_with_lengths& MC = GL.MC;

  const unsigned n_edges = MC.edges.size();

  const unsigned n_nodes = MC.n_nodes();

  ublas::matrix<int> n_cross(n_nodes,n_nodes);
  for(int i=0;i<n_nodes;i++)
    for(int j=0;j<n_nodes;j++)
      n_cross(i,j) = 0;

  for(int i=0;i<n_edges;i++)
    if (MC.edges[i].type == 1)
      for(int j=0;j<n_edges;j++)
	if (MC.edges[j].type == 1)
	  if (e_cross(i,j)) 
	  {
	    int b1 = MC.edges[i].partition;
	    int b2 = MC.edges[j].partition;
	    
	    //	    cerr<<"A: b1 = "<<b1<<"   b2 = "<<b2<<endl;

	    if (MC.left_of(b1,b2) or MC.left_of(b1,MC.reverse(b2)))
	      b1 = MC.reverse(b1);
	    if (MC.left_of(b2,b1) or MC.left_of(b2,MC.reverse(b1)))
	      b2 = MC.reverse(b2);
	    if (not MC.left_of(MC.reverse(b1),b2)) continue;
	    
	    //	    cerr<<"B: b1 = "<<b1<<"   b2 = "<<b2<<endl;

	    assert(MC.left_of(MC.reverse(b1),b2));
	    assert(MC.left_of(MC.reverse(b2),b1));
	    
	    vector<int> nodes_after1 = nodes_after(MC,b1);
	    vector<int> nodes_after2 = nodes_after(MC,b2);
	    
	    for(int k=0;k<nodes_after1.size();k++)
	      for(int l=0;l<nodes_after2.size();l++)
		n_cross(nodes_after1[k],nodes_after2[l]) = 1;
	  }

  return n_cross;
}


vector<bool> crossed_nodes_vector(const graph_layout& GL)
{
  ublas::matrix<int> n_cross = crossed_nodes_matrix(GL);

  vector<bool> n_cross_v(n_cross.size1(),false);

  for(int i=0;i < n_cross.size1(); i++)
    for(int j=0;j < n_cross.size2(); j++)
      if (n_cross(i,j)) {
	n_cross_v[i] = true;
	n_cross_v[j] = true;
      }

  return n_cross_v;  
}

       

struct energy2: public graph_energy_function
{
  double repulsion_weight;
  double length_weight;
  double down_weight_stretchy;
  double angle_weight;

  double operator()(const graph_layout& GL) const 
  {
    vector<point_position> D(GL.MC.n_nodes());
    return operator()(GL,D);
  }


  // How should the repulsion terms be SCALED,
  //   if they are to give a layout as well as avoiding collisions?
  double operator()(const graph_layout& GL, vector<point_position>& D) const 
  {
    const MC_tree_with_lengths& MC = GL.MC;
    
    const unsigned n_edges = MC.edges.size();
    const unsigned n_nodes = MC.n_nodes();

    //--- check all edge pairs to see if they cross ---//
    //    ublas::matrix<int> n_cross = crossed_nodes_matrix(GL);

    //--------------- resize and zero D ---------------//
    if (D.size() != n_nodes) D.resize(n_nodes);
    for(int i=0;i<D.size();i++)
      D[i].x = D[i].y = 0;

    double E = 0;

    /// edge length energies (type 1)
    const double closest_fraction = 1.0;

    // O(E)
    for(int e=0;e<GL.MC.edges.size();e++) 
    {
      int n1 = GL.MC.edges[e].from;
      int n2 = GL.MC.edges[e].to;
      int t = GL.MC.edges[e].type;

      double L = 0;
      double w = length_weight;
      if (t == 1) {
	int b = GL.MC.edges[e].partition;
	L = GL.MC.branch_length(b);
	//	w /= L;
      }
      else {
	int b = GL.MC.branch_to_node(GL.MC.edges[e].from);
	w /= down_weight_stretchy;
	w *= 1.0/(GL.MC.directly_wanders[b] + 1);
	w *= (1.0-closest_fraction);
      }
      E += node_node_attraction(GL, D, n1, n2, w, L);
    }

    /// node_distances
    // O(n*n)
    for(int n1=0;n1<GL.MC.n_nodes();n1++) 
      for(int n2=0;n2<n1;n2++) 
	E += node_node_repulsion(GL, D, n1, n2, repulsion_weight);
    
    /// edge length energies (type 2)
    // consider - find the closest edge OR NODE (with radius!) - may be smoother/better
    for(int b = 0;b<GL.MC.n_branches()*2;b++) 
    {
      if (not GL.MC.directly_wanders[b]) continue;
	
      int n1 = GL.MC.mapping[b];

      vector<double> distances;
      vector<int> branches;
      for(int i = 0;i<GL.MC.branch_order.size();i++) 
      {
	int b2 = GL.MC.branch_order[i];

	if (GL.MC.directly_wanders_over(b,b2)) 
	{
	  int n2 = GL.MC.mapping[b2];
	  int n3 = GL.MC.mapping[GL.MC.reverse(b2)];
	
	  double d = distance_to_line_segment(GL,n1,n2,n3);

	  branches.push_back(b2);
	  distances.push_back(d);
	}
      }

      int i = argmin(distances);
      int b2 = branches[i];
      int n2 = GL.MC.mapping[b2];
      int n3 = GL.MC.mapping[GL.MC.reverse(b2)];

      double w = closest_fraction*length_weight/down_weight_stretchy;

      double L = GL.MC.branch_length(b);
      
      //      w /= L;
      
      E += node_edge_attraction(GL, D, n1, n2, n3, w);
    }

    // angular resolution
    if (angle_weight > 0)
    for(int i=0;i<GL.MC.n_nodes();i++) {
      vector<int> neighbors;
      for(int j=0;j<GL.MC.n_nodes();j++)
	if (GL.MC.connected(i,j) or GL.MC.connected(j,i))
	  neighbors.push_back(j);

      if (neighbors.size() < 2) continue;

      vector<double> angles(neighbors.size());
      for(int k=0;k<neighbors.size();k++) {
	int j = neighbors[k];
	double dx = GL.node_positions[j].x - GL.node_positions[i].x;
	double dy = GL.node_positions[j].y - GL.node_positions[i].y;
	angles[k] = atan2(dy,dx);
      }
      vector<int> order = iota<int>(neighbors.size());

      std::sort(order.begin(),order.end(),sequence_order<double>(angles));

      for(int k=0;k<order.size();k++)
      {
	int l = (k+1)%order.size();
	int o1 = order[k];
	int o2 = order[l];

	int n1 = neighbors[o1];
	int n2 = neighbors[o2];

	double a1 = angles[o1];
	double a2 = angles[o2];

	double theta1 = abs(angle_difference(a1,a2));

	E += angle_energy(GL,D,n1,n2,i,theta1,angle_penalty,angle_deriv,angle_weight);
      }
    }

    return E;
  }

  energy2(double W1, double W2,double W3,double W4)
    :repulsion_weight(W1),
     length_weight(W2),
     down_weight_stretchy(W3),
     angle_weight(W4)
  {}
};

// idea #3 - use Kamada-Kawai-type distances, but choose the 'closest' attatchment point to
//           compute the distance across a stretchy branch.another ide

void inc(vector<point_position>& x1,double lambda,const vector<point_position>& v)
{
  for(int i=0;i<x1.size();i++) {
    x1[i].x += lambda * v[i].x;
    x1[i].y += lambda * v[i].y;
    x1[i].z += lambda * v[i].z;
  }
}

vector<point_position> add(const vector<point_position>& x1,double lambda,const vector<point_position>& v)
{
  vector<point_position> x2 = x1;
  inc(x2,lambda,v);
  return x2;
}

double dot(const vector<point_position>& v1,const vector<point_position>& v2)
{
  double T=0;
  for(int i=0;i<v1.size();i++) {
    T += v1[i].x * v2[i].x;
    T += v1[i].y * v2[i].y;
  }
  return T;
}


vector<point_position> energy_derivative_2D(const graph_layout& GL1, const graph_energy_function& E)
{
  double DELTA = 0.000001;

  graph_layout GL2 = GL1;

  vector<point_position> D(GL1.MC.n_nodes());

  for(int i=0;i<GL1.MC.n_nodes();i++) 
  {
    double temp = GL2.node_positions[i].x;
    GL2.node_positions[i].x = temp - DELTA;
    double E1 = E(GL2);
    GL2.node_positions[i].x = temp + DELTA;
    double E2 = E(GL2);
    GL2.node_positions[i].x = temp;
    D[i].x = (E2-E1)/(2.0*DELTA);

    temp = GL2.node_positions[i].y;
    GL2.node_positions[i].y = temp - DELTA;
    E1 = E(GL2);
    GL2.node_positions[i].y = temp + DELTA;
    E2 = E(GL2);
    GL2.node_positions[i].y = temp;
    D[i].y = (E2-E1)/(2.0*DELTA);
  }

  return D;
}

double max_delta(const vector<point_position>& p)
{
  double m = std::max(std::abs(p[0].x),std::abs(p[0].y));
  for(int i=1;i<p.size();i++) {
    m = std::max(m,std::abs(p[i].x));
    m = std::max(m,std::abs(p[i].y));
  }
  return m;
}

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/kamada_kawai_spring_layout.hpp>
#include <boost/graph/fruchterman_reingold.hpp>

using namespace boost;

typedef adjacency_list< vecS, vecS, bidirectionalS> Graph; 
typedef graph_traits<Graph>::vertex_descriptor Vertex;
typedef graph_traits<Graph>::edge_descriptor Edge_t;


graph_layout kamada_kawai_layout(graph_layout GL)
{
  const MC_tree& MC = GL.MC;
  Graph g(MC.n_nodes());

  //------------ Construct the Graph --------------//
  for(int i=0;i<MC.edges.size();i++) {
    const mc_tree_edge& e = MC.edges[i];
    add_edge(e.from, e.to, g);
  }

  //--------- Package position as property ---------//
  typedef vector<point_position> PositionVec;
  typedef iterator_property_map<PositionVec::iterator,
                                property_map<Graph, vertex_index_t>::type>
    PositionMap;

  PositionMap position(GL.node_positions.begin(), get(vertex_index, g));

  //--------- Package weights as property ---------//
  vector<double> weights(num_edges(g));

  //  typedef vector<double> WeightVec;
  //  typedef iterator_property_map<WeightVec::iterator, 
  //    property_map<Graph, edge_index_t>::type>
  //    WeightMap;

  //  WeightMap weight(weights.begin(), get(edge_index,g));

  // kamada_kawai_spring_layout(g, position, weight, boost::side_length(1.0) );

  return GL;
}

struct my_square_distance_attractive_force {
  template<typename Graph, typename T>
  T
  operator()(typename graph_traits<Graph>::edge_descriptor,
             T k,
             T d,
             const Graph&) const
  {
    k /= 10;
    d -= k;
    double temp = 100.0*d * d / k;
    if (d < k) temp *= -1;
    return temp;
  }
};


#include <boost/graph/random_layout.hpp>
#include <boost/random/linear_congruential.hpp>

graph_layout fruchterman_reingold_layout(graph_layout GL, double width, double height)
{
  /*
  const MC_tree& MC = GL.MC;
  Graph g(MC.n_nodes());

  //------------ Construct the Graph --------------//
  for(int i=0;i<MC.edges.size();i++) {
    const mc_tree_edge& e = MC.edges[i];
    add_edge(e.from, e.to, g);
  }

  //--------- Package position as property ---------//
  typedef vector<point_position> PositionVec;
  typedef iterator_property_map<PositionVec::iterator,
                                property_map<Graph, vertex_index_t>::type>
    PositionMap;

  PositionMap position(GL.node_positions.begin(), get(vertex_index, g));

  cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;

  //------------ Initial random layout ------------//
  minstd_rand gen;
  gen.seed(rng::get_random_seed());
  rectangle_topology<> R(-width/2, width/2, -height/2, height/2);
  random_graph_layout(g, position, R);
  cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;

  //------------ Final force-directed layout ------------//
  //  width *= MC.n_branches()*100;
  //  height *= MC.n_branches()*100;
  width *= 100;
  height *= 100;

  PositionVec Displacements(num_vertices(g));
  PositionMap displacements(Displacements.begin(), get(vertex_index, g));

  fruchterman_reingold_force_directed_layout(g, 
					     position, 
					     R,
					     my_square_distance_attractive_force(),
					     square_distance_repulsive_force(),
					     make_grid_force_pairs(R,position,g),
					     linear_cooling<double>(1000),
					     displacements
					     );

  cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;
  */
  return GL;
}

void energy_layout(graph_layout& GL, const graph_energy_function& E,int n=1000)
{
  double T = 0;
  int successes=0;

  vector<point_position> D;
  //  double E = E(GL,D);

  double dt = 1;//E(GL,D)/dot(D,D);

  for(int i=0;i<n;i++) 
  {
    double E1 = E(GL,D);
    assert(E1 >= 0);
    if (log_verbose) 
      cerr<<"iter = "<<i<<" E = "<<E1<<"   T = "<<T<<"   dt = "<<dt<<"   edge_length_error = "<<GL.edge_length_error()<<endl;

    if (log_verbose)
      cerr<<"  x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;

    /*
    // check D versus D2
    vector<point_position> D2 = energy_derivative_2D(GL,E);
    cerr<<"      derivative error = "<<sqrt(dot(D,D)/dot(D2,D2))
	<<"      angle = "<<dot(D2,D)/sqrt(dot(D2,D2)*dot(D,D))<<"\n";
    //    D = D2;
    */


    // problem with these equations is STIFFness (? and rotation?)


    // Make a scaled version
    vector<point_position> DD = D;
    for(int i=0;i<DD.size();i++) 
    {
      double v = sqrt(DD[i].x*DD[i].x + DD[i].y*DD[i].y);
      DD[i].x /= v;
      DD[i].y /= v;
    }

    vector<point_position> temp = GL.node_positions;
    inc(GL.node_positions,-dt,DD);

    double E2 = E(GL);
    if (E2 > E1) {
      if (log_verbose) cerr<<"draw-tree:    rejecting  E = "<<E2<<endl;
      GL.node_positions = temp;
      dt /= 2.0;
      successes=0;
    } 
    else {
      T += dt;
      successes++;
      if (successes>3) {
	dt *= 2.0;
	if (dt > 1.0) dt = 1.0;
	successes=0;
      }

      // actually, we need an estimate of the CURVATURE to know when to stop!
      if (max_delta(GL.node_positions)/std::min(GL.x_width(),GL.y_width()) < 0.00001)
	return;

      // (how to go from 'energy near bottom' to 'position near position at lowest energy',
      //  which is what really matters?

    }
  }
}

// FIXME: construct child_nodes
// FIXME: construct color so that no clouds with overlapping child nodes have the same color.
//        Algorithm: Welsh and Powell
//                   1. Sort clouds by number of neighbors
//                   2. Color with smallest color that is unused by neighbors.

struct cloud 
{
  vector<int> child_nodes;     // FIXME - compute child nodes!

  vector<int> child_branches;  // These UNDIRECTED branches are wandered over.

  vector<int> parent_branches; // These DIRECTED branches wander DIRECTLY over this cloud;

  int depth;

  int color;

  vector<int> neighbors;

  int degree() const { return neighbors.size();}

  int size() const {return child_branches.size();}

  cloud():depth(0) {}
  cloud(const vector<int>& v): child_branches(v),depth(0),color(-1) {}
  cloud(const vector<int>& v,int p): child_branches(v),parent_branches(1,p),depth(0) {}
};

bool overlap(const vector<int>& v1, const vector<int>& v2)
{
  for(int i=0;i<v1.size();i++)
    for(int j=0;j<v2.size();j++)
      if (v1[i] == v2[j])
	return true;
  return false;
}

bool overlap(const cloud& c1, const cloud& c2)
{
  return overlap(c1.child_branches, c2.child_branches);
}


struct degree_order
{
  const vector<cloud>& clouds;
  bool operator()(int i,int j) const {
    return clouds[i].degree() > clouds[j].degree();
  }

  degree_order(const vector<cloud>& v):clouds(v) {}
};

vector< cloud > get_clouds(const MC_tree& MC)
{
  vector<cloud> clouds;

  // Consider each (ordered) branch...
  for(int b=0;b<MC.partitions.size();b++)
  {
    vector<int> child_branches;

    // ... to see what (unordered) branches it wanders over.
    for(int i=0;i<MC.branch_order.size();i++)
    {
      int b2 = MC.branch_order[i];
      if (MC.directly_wanders_over(b,b2))
	child_branches.push_back(i);
    }

    if (not child_branches.size()) continue;

    //How many branches does this branch wander over?
    //cerr<<"size = "<<child_branches.size()<<endl;

    // Search the clouds found so far... which should be in order of increasing size
    int which = -1;
    int larger = -1;
    for(int i=0;i<clouds.size();i++) {
      if (clouds[i].child_branches == child_branches)
	which = i;

      // select the FIRST (and therefore SMALLEST) cloud that is larger than this one.
      if (clouds[i].size() > child_branches.size() and larger == -1)
	larger = i;
    }

    //The first clouds that is larger than this one
    //cerr<<"larger = "<<larger<<endl;
    //cerr<<endl;

    // NOTE: clouds should be sorted by size.
    if (which == -1)
    { 
      cloud C(child_branches,b);

      if (larger == -1)  	// add new cloud at end
	clouds.push_back(C);
      else                      // insert new cloud before larger cloud
	clouds.insert(clouds.begin()+larger,C);
    }
    else // add to parent list of existing cloud
      clouds[which].parent_branches.push_back(b);
  }  

  // Add branch i to consistent 0:i-1 other branches
  for(int i=0;i<clouds.size();i++)
    for(int j=0;j<i;j++)
      if (overlap(clouds[i],clouds[j]))
	clouds[i].depth = clouds[j].depth + 1;


  // Find child nodes
  for(int i=0;i<clouds.size();i++)
  {
    for(int j=0;j<clouds[i].child_branches.size();j++)
    {
      int b = clouds[i].child_branches[j];

      int n1 = MC.edges[b].from;
      int n2 = MC.edges[b].to;

      if (not includes(clouds[i].child_nodes,n1))
	clouds[i].child_nodes.push_back(n1);
      if (not includes(clouds[i].child_nodes,n2))
	clouds[i].child_nodes.push_back(n2);
    }
  }

  // Find neighbors (clouds that cannot have the same color)
  for(int i=0;i<clouds.size();i++)
    for(int j=0;j<i;j++) 
      if (overlap(clouds[i].child_nodes, clouds[j].child_nodes))
      {
	clouds[i].neighbors.push_back(j);
	clouds[j].neighbors.push_back(i);
      }


  // Sort list to have decreasing number of neighbors
  vector<int> order = iota<int>(clouds.size());

  degree_order D(clouds);

  std::sort(order.begin(), order.end(), D);

  // Color nodes to have different colors than all their neighbors.
  for(int i=0;i<order.size();i++) 
  {
    cloud& c = clouds[order[i]];

    int color = -1;
    while(1)
    {
      color++;
      bool ok = true;
      for(int j=0;j<c.neighbors.size() and ok;j++)
	if (clouds[c.neighbors[j]].color == color)
	  ok = false;
      if (ok) break;
    }

    c.color = color;
  }


  return clouds;
}


int find_cloud(const vector<cloud>& clouds,int b)
{
  for(int c=0;c<clouds.size();c++)
    if (includes(clouds[c].parent_branches,b))
      return c;

  return -1;
}

struct graph_plotter: public cairo_plotter
{
  graph_layout L;
  void operator()(cairo_t*);

  bool draw_clouds;

  bool draw_type_2_edges;

  graph_plotter(const graph_layout& gl,double xw,double yw, double fs)
    :cairo_plotter(xw,yw,fs),
     L(gl),
    draw_clouds(false),
    draw_type_2_edges(true)
  {}
};

void cairo_make_cloud_branch_path(cairo_t* cr,int b, const double W,const graph_layout& GL,
				  vector<int>& nodes_visited)
{
  int n1 = GL.MC.edges[b].from;
  int n2 = GL.MC.edges[b].to;

  nodes_visited[n1]++;
  nodes_visited[n2]++;

  double x1 = GL.node_positions[n1].x;
  double y1 = GL.node_positions[n1].y;

  double x2 = GL.node_positions[n2].x;
  double y2 = GL.node_positions[n2].y;

  double dx = x2-x1;
  double dy = y2-y1;

  double L = sqrt(dx*dx + dy*dy);

  cairo_save(cr);
  cairo_translate(cr,x1,y1);

  // begins a new sub-path (e.g. a discontinuous part of the path
  cairo_move_to(cr,0,0);
  cairo_rotate(cr, atan2(dy,dx));
    
  cairo_move_to(cr, 0, 0.5*W);
  cairo_line_to(cr, L, 0.5*W);
  //	    cairo_line_to(cr, 1, -0.5);
  cairo_arc_negative(cr, L, 0.0, 0.5*W, M_PI/2.0,-M_PI/2.0);
  cairo_line_to(cr, 0, -0.5*W);
  //	    cairo_line_to(cr, 0, 0.5*W);
  cairo_arc_negative(cr, 0.0 , 0.0, 0.5*W, -M_PI/2.0,M_PI/2.0);
  
  cairo_close_path(cr);
  
  cairo_restore(cr);
}

// Create a cairo "path" (possibly with disconnected components -- "sub-paths") 
//   reprenting the cloud @C.
// This path can then be stroked, or filled, or used to clip, or whatever.
void cairo_make_cloud_path(cairo_t* cr,const cloud& C,const double line_width, const graph_layout& L) 
{
  int depth = C.depth;

  double W = line_width*(4+depth*2);

  vector<int> nodes_visited(L.node_positions.size(),0);

  for(int i=0;i<C.child_branches.size();i++) 
  {
    int b = C.child_branches[i];
    cairo_make_cloud_branch_path(cr,b,W,L,nodes_visited);
  }

  for(int n=0;n<nodes_visited.size();n++) 
  {
    if (nodes_visited[n] and (nodes_visited[n]%2==0)) 
    {
      //      cerr<<"node "<<n<<": visited "<<nodes_visited[n]<<" times."<<endl;
      double x = L.node_positions[n].x;
      double y = L.node_positions[n].y;

      cairo_new_sub_path(cr);
      cairo_arc(cr, x, y, 0.5*W, 0, 2*M_PI);    
      cairo_close_path(cr);
    }
  }

}

void cairo_add_clip_extents_as_path(cairo_t* cr)
{
  double x1;
  double y1;
  double x2;
  double y2;

  cairo_clip_extents (cr, &x1, &y1, &x2, &y2);

  //  cerr<<"clip extents ("<<x1<<","<<y1<<") - ("<<x2<<","<<y2<<")\n";

  cairo_move_to(cr,x1,y1);
  cairo_line_to(cr,x2,y1);
  cairo_line_to(cr,x2,y2);
  cairo_line_to(cr,x1,y2);
  cairo_close_path(cr);
}

void graph_plotter::operator()(cairo_t* cr)
{
  vector<bool> e_cross_v = edge_crossing_vector(L);

  //  ublas::matrix<int> n_cross = crossed_nodes_matrix(L);

  //  vector<bool> n_cross_v = crossed_nodes_vector(L);

  double xc = 0.5*(L.xmin() + L.xmax());
  double yc = 0.5*(L.ymin() + L.ymax());
  double xw = L.x_width();
  double yw = L.y_width();

  //  cout<<"x = ["<<L.xmin()<<", "<<L.xmax()<<"]"<<endl;
  //  cout<<"y = ["<<L.ymin()<<", "<<L.ymax()<<"]"<<endl;
  //  cout<<endl;

  // move to center
  cairo_translate(cr, page_x_width()*inch/2.0, page_y_width()*inch/2.0);

  // find scaling factor
  const double edge = 0.10;
  const double factor = 1.0-edge*2;

  double scale = std::min(page_x_width()*inch/xw,page_y_width()*inch/yw);
  scale *= factor;
  cairo_scale(cr, scale, scale);
  cairo_translate(cr, -xc, -yc);
  
  const double pt = 1.0/scale;
  const double line_width = 2.0*pt;
  const double dashes[] = {3.0*line_width, 3.0*line_width};
  cairo_set_line_width(cr, line_width);

  vector<cloud> clouds = get_clouds(L.MC);
  if (draw_clouds)
  {
    if (log_verbose) cerr<<"draw-tree: Got "<<clouds.size()<<" clouds.\n";
    
    for(int c=clouds.size()-1;c>=0;c--)
    {
      if (log_verbose) cerr<<"draw-tree: "<<c+1<<":  depth = "<<clouds[c].depth<<"   size = "<<clouds[c].size()<<"\n";
      int D = clouds[c].depth;
      int C = clouds[c].color;
      if (log_verbose) {
	cerr<<"draw-tree:  degree : "<<clouds[c].degree()<<endl;
	cerr<<"draw-tree:  color  : "<<clouds[c].color<<endl;
	cerr<<"draw-tree:     #b : "<<clouds[c].child_branches.size()<<endl;
	cerr<<"draw-tree:     #n : "<<clouds[c].child_nodes.size()<<endl;

	for(int i=0;i<clouds[c].child_nodes.size();i++)
	  cout<<"draw-tree:   node : "<<clouds[c].child_nodes[i]<<endl;
      }
      for(int i=0;i<clouds[c].size();i++)
      {
	int e = clouds[c].child_branches[i];
	if (log_verbose) cerr<<"draw-tree:   edge : "<<e<<"\n";
	
	int n1 = L.MC.edges[e].from;
	int n2 = L.MC.edges[e].to;
	int t =  L.MC.edges[e].type;
	int b =  L.MC.edges[e].partition;
	
	double x1 = L.node_positions[n1].x;
	double y1 = L.node_positions[n1].y;
	
	double x2 = L.node_positions[n2].x;
	double y2 = L.node_positions[n2].y;
	cairo_save(cr); 
	{
	  cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND); 
	  cairo_move_to (cr, x1, y1);
	  cairo_line_to (cr, x2, y2);
	  
	  //	  double L = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
	  cairo_set_line_width(cr, line_width*(4+D*2) );
	  
	  if (C == 0)
	    cairo_set_source_rgb (cr, 1 , 0.80, 0.80);
	  else if (C == 1)
	    cairo_set_source_rgb (cr, 0.80 , 0.80, 1);
	  else if (C == 2)
	    cairo_set_source_rgb (cr, 0.80 , 1, 0.80);
	  else if (C == 3)
	    cairo_set_source_rgb (cr, 0.80 , 1, 1);
	  else if (C == 4)
	    cairo_set_source_rgb (cr, 1, 1, 0.80);
	  else if (C == 5)
	    cairo_set_source_rgb (cr, 1, 0.80, 1);
	  
	  cairo_stroke (cr);
	}
	cairo_restore(cr);
      }
    }
  }
  
  // draw the edges
  for(int e=0;e<L.MC.edges.size();e++) 
  {
    int n1 = L.MC.edges[e].from;
    int n2 = L.MC.edges[e].to;
    int t =  L.MC.edges[e].type;
    int b =  L.MC.edges[e].partition;
    
    double x1 = L.node_positions[n1].x;
    double y1 = L.node_positions[n1].y;

    double x2 = L.node_positions[n2].x;
    double y2 = L.node_positions[n2].y;

    if (t == 2 and not draw_type_2_edges) continue;

    cairo_save(cr); 
    {
      if (t == 1)
      {
	if (L.MC.partitions[b].full())
	  cairo_set_source_rgb (cr, 0, 0 ,0);
	else
	  cairo_set_source_rgb (cr, 0.3, 1.0 ,0.3);

	// pointing the other way
	int b2 = L.MC.reverse(b);

	int c1 = find_cloud(clouds,b);
	int c2 = find_cloud(clouds,b2);

	//	cairo_reset_clip(cr);

	/// Only clip if we are not drawing type 2 edges
	if ((c1 != -1 or c2 != -1) and draw_clouds and not draw_type_2_edges) 
	{
	  /*
	  cairo_save(cr);
	  cairo_make_cloud_path(cr, clouds[c1], line_width,L);

	  cairo_set_source_rgb (cr, 0 , 0, 1);
	  cairo_set_line_width(cr, line_width*2);
	  //	  cairo_set_dash (cr, dashes, 2, 0.0);
	  cairo_stroke(cr);
	  cairo_restore(cr);
	  */

	  // so this must be clockwise, in order to work?
	  cairo_add_clip_extents_as_path(cr);

	  if (c1 != -1)
	    cairo_make_cloud_path(cr, clouds[c1], line_width,L);

	  if (c2 != -1)
	    cairo_make_cloud_path(cr, clouds[c2], line_width,L);

	  cairo_clip(cr);
	}
	
	// Um, but BOTH ends might wander!
	// clip to all the ONE cloud that we wander over
      }
      else //(t == 2)
      {
	cairo_set_line_width(cr, line_width/2.0);
	cairo_set_dash (cr, dashes, 2, 0.0);
      }

      if (false) // if (e_cross_v[e])
	cairo_set_source_rgb (cr, 1 , 0, 0);
      
      cairo_move_to (cr, x1, y1);
      cairo_line_to (cr, x2, y2);

      cairo_stroke (cr);
    }
    cairo_restore(cr);
  }

  cairo_set_dash (cr, dashes, 2, 0.0);

  // draw the circles
  for(int n=0;n<L.node_radius.size();n++) 
  {
    double x = L.node_positions[n].x;
    double y = L.node_positions[n].y;

    if (L.node_radius[n] > 0) {
      cairo_save(cr);
      RGB& color = L.node_color[n];
      //cerr<<"node n="<<n<<"  radius = "<<L.node_radius[n]<<endl;
      cairo_arc(cr, x, y, L.node_radius[n], 0.0, 2.0 * M_PI);

      cairo_set_source_rgb (cr, color.R , color.G, color.B);
      cairo_fill_preserve (cr);

      cairo_set_source_rgb (cr, 0 , 0, 0);
      cairo_set_line_width(cr, line_width/2.0);
      cairo_stroke(cr);
      cairo_restore(cr);
    }

    cairo_save(cr);
    if (false) { // if (n_cross_v[n])
      //      cerr<<"n_cross: "<<n<<endl;
      double R = 0.001;
      cairo_arc(cr, x, y, R, 0.0, 2.0 * M_PI);

      cairo_set_source_rgb (cr, 0 , 0, 1);
      cairo_fill (cr);

      //      cairo_set_source_rgb (cr, 0 , 0, 0);
      //      cairo_set_line_width(cr, line_width/2.0);
      //      cairo_stroke(cr);
    }
    cairo_restore(cr);
  }

  cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
  cairo_set_font_size (cr, font_size()/scale);

  for(int l=0;l<L.MC.n_leaves();l++)
  {  
    int n = L.MC.leaf(l);
    string name = L.MC.names()[l];
    int b = L.MC.branch_to_node(n);
    int n1 = L.MC.mapping[b];
    int n2 = L.MC.mapping[L.MC.reverse(b)];

    double x1 = L.node_positions[n1].x;
    double y1 = L.node_positions[n1].y;

    double x2 = L.node_positions[n2].x;
    double y2 = L.node_positions[n2].y;

    double a = atan2(y1-y2,x1-x2);

    double W = get_text_length(cr, name);
    double H = get_text_height(cr, name);

    x1 += cos(a)*(line_width+H/8);
    y1 += sin(a)*(line_width+H/2);

    y1 += 0.5*H;

    if (cos(a) < 0) x1 -= W;

    cairo_move_to (cr, x1, y1);
    cairo_show_text (cr, name.c_str());
  }

    
 
}

void draw_graph(const MC_tree_with_lengths& T,const string& name)
{
  // try to scale lengths in the *.dot file so that the average is 1.0
  double scale = 1.0;
  double total_branch_length = 0;
  for(int b=0;b<T.n_branches();b++)
    total_branch_length += T.branch_length(b);
  scale = 0.5*T.n_branches()/total_branch_length;

  const int N = T.n_nodes();

  cout<<"digraph "<<name<<" { \n\
\n\
      nodesep=1.0\n\
      ratio=auto\n\
\n\
      edge[style=\"setlinewidth(2)\"]\n\
      node[shape=plaintext,width=auto,fontname=Helvitica,fontsize=10]\n\n";

  // edges
  for(int i=0;i<T.edges.size();i++) 
  {
    const mc_tree_edge& e = T.edges[i];
    cout<<"      N"<<e.from<<" -> N"<<e.to;
    int b = e.partition;

    vector<string> attributes;
    vector<string> styles;

    if (e.type == 1) {
      attributes.push_back("arrowhead=none");
      attributes.push_back(string("len=")+convertToString(scale*T.branch_length(b)));
      const Partition& p = T.partitions[e.partition];
      if (informative(p))
	styles.push_back("setlinewidth(2)");
      if (not p.full())
	attributes.push_back("color=green2");
    }
    else {
      styles.push_back("dashed");
      int b = T.branch_to_node(e.from);
      double w = 1.0/(T.directly_wanders[b] + 1);
      attributes.push_back(string("weight=")+convertToString(w));
      attributes.push_back(string("w=")+convertToString(w));
    }

    if (styles.size()) {
      string style = "style=\"" + join(styles,',') + "\"";
      attributes.push_back(style);
    }
    
    cout<<" ["<<join(attributes,',')<<"]";
    cout<<"\n";
  }
  cout<<endl;

  // leaf names
  vector<string> names = T.names();
  for(int i=0;i<names.size();i++)
    cout<<"      N"<<T.leaf(i)<<" [label=\""<<names[i]<<"\"]\n";
  cout<<endl;

  for(int n=0;n<N;n++) 
  {
    if (not T.is_leaf_node(n)) {
      int d = T.degree(n);
      double D = node_diameter(T.node_length(n),d)/2.0;
      D *= 5.0;
      if (D == 0.0)
	cout<<"      N"<<n<<" [label=\"\",shape=circle,height=0.02,width=0.02,fontsize=1]\n";
      else 
	cout<<"      N"<<n<<" [label=\"\",shape=circle,style=dashed,height="<<D<<",width="<<D<<",fontsize=1]\n";
    }
  }

  cout<<endl;

  cout<<"}\n";

}

MC_tree_with_lengths collapse_MC_tree(const MC_tree_with_lengths& MC1)
{
  vector<Partition> full;
  for(int i=0;i<MC1.branch_order.size();i++) {
    int b = MC1.branch_order[i];
    if (MC1.partitions[b].full())
      full.push_back(MC1.partitions[b]);
  }

  MC_tree_with_lengths MC2(full);
  

  // what node in MC2 do nodes in MC1 map to?
  vector<int> node_mapping(MC1.n_nodes(),-1);
  vector<int> branch_mapping(MC1.n_branches(),-1);

  // map full branches in MC1 -> MC2
  for(int i1=0;i1<MC1.n_branches();i1++)
  {
    int b1 = MC1.branch_order[i1];

    if (not MC1.partitions[b1].full()) continue;

    int b2 = find_index(MC2.partitions,MC1.partitions[b1]);
    if (b2 == -1) throw myexception()<<"Can't find full partition in collapsed tree!\n"<<MC1.partitions[b1];

    branch_mapping[i1] = b2;

    int left1 = MC1.mapping[MC1.reverse(b1)];
    int right1 = MC1.mapping[b1];

    int left2 = MC2.mapping[MC2.reverse(b2)];
    int right2 = MC2.mapping[b2];

    node_mapping[right1] = right2;
    node_mapping[left1] = left2;
  }

  // map partial branches in MC1 -> MC2 (slowly)
  vector<int> active_branches = iota(2*MC1.n_branches());
  while (active_branches.size())
  {
    vector<int> branches2;

    for(int i=0;i<active_branches.size();i++)
    {
      int b = active_branches[i];
      int left = MC1.mapping[MC1.reverse(b)];
      int right = MC1.mapping[b];

      if (node_mapping[left] != -1 and node_mapping[right] == -1) {
	node_mapping[right] = node_mapping[left];
      }
      else if (node_mapping[left] == -1 and node_mapping[right] == -1)
	branches2.push_back(b);
    }
    active_branches = branches2;
  }


  // accumulate branch lengths from MC1 -> MC2
  for(int i1=0;i1<MC1.n_branches();i1++) 
  {
    int b1 = MC1.branch_order[i1];
    int b2 = branch_mapping[i1];

    // cerr<<"b1 = "<<b1<<" -> "<<b2<<endl;
    if (b2 != -1) {
      int b2r = MC2.reverse(b2);
      MC2.branch_length(b2) = MC2.branch_length(b2r) = MC1.branch_length(b1);
    }
    else {
      int left1 = MC1.mapping[MC1.reverse(b1)];
      int right1 = MC1.mapping[b1];
      
      assert(node_mapping[left1] == node_mapping[right1]);
      
      int node = node_mapping[left1];
      MC2.node_length(node) += MC1.branch_length(b1);
    }
  }

  // accumulate node lengths from MC1 -> MC2
  for(int n=0;n<MC1.n_nodes();n++) 
  {
    assert(n != -1);
    MC2.node_length(node_mapping[n]) += MC1.node_length(n);
  }

  // sync the MC2 branch lengths with the tree MC2.T
  for(int b=0;b<MC2.T.n_branches();b++)
    MC2.T.branch(b).set_length(MC2.branch_length(b));

  return MC2;
}

void draw(string filename,const string& type,cairo_plotter& cp)
{
  if (type == "pdf")
    filename = filename + ".pdf";
  else if (type == "ps")
    filename = filename + ".ps";
  else if (type == "svg") 
    filename = filename + ".svg";
  else
    throw myexception()<<"I don't recognize output type '"<<type<<" - type \'ps\', \'pdf\', or \'svg\'";

  if (type == "pdf")
    draw_to_pdf(filename,cp);
  else if (type == "ps")
    draw_to_ps(filename,cp);
  else if (type == "svg")
    draw_to_svg(filename,cp);
}

MC_tree_with_lengths collapse_nodes(const MC_tree_with_lengths& MC1)
{
  MC_tree_with_lengths MC2 = MC1;
  for(int n=0;n<MC2.n_nodes();n++) 
  {
    // find branches pointing to node
    vector<int> branches;
    for(int b=0;b<2*MC2.n_branches();b++)
      if (MC2.mapping[b] == n)
	branches.push_back(b);

    // divide node lengths among branches
    for(int i=0;i<branches.size();i++) {
      int b1 = branches[i];
      int b2 = MC2.reverse(b1);
      MC2.branch_length(b1) += MC2.node_length(n)/branches.size();
      MC2.branch_length(b2) = MC2.branch_length(b1);
      if (MC2.partitions[b1].full())
	MC2.T.directed_branch(b1).set_length(MC2.branch_length(b1));
    }

    // clear node length
    MC2.node_length(n) = 0;
  }

  return MC2;
}


int main(int argc,char* argv[]) 
{
  try {
#if defined(HAVE_FENV_H) && !defined(NDEBUG)
    feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    
    double xw = args["width"].as<double>();
    double yw = args["height"].as<double>();
    double font_size = args["font-size"].as<double>();

    string output = args["output"].as<string>();


    //-------- Load MC Tree and lengths -----//
    string filename = args["file"].as<string>();
    string name = get_graph_name(filename);

    // FIXME! - load a MF/MC tree w/ no length information
    // FIX name collision!
    MC_tree_with_lengths MC = get_MC_tree_with_lengths(filename);
    if (log_verbose) {
      cerr<<"score = "<<MC.score()<<"    "<<double(MC.score())/MC.n_leaves()<<" / "<<MC.n_leaves()-3<<" branches\n";
    }

    if (args.count("full"))
      MC = collapse_MC_tree(MC);

    for(int i=0;i<MC.n_nodes();i++)
      assert(not isnan(MC.node_length(i)));


    if (args.count("collapse"))
      MC = collapse_nodes(MC);

    if (output == "dot")
    {
      draw_graph(MC,get_graph_name(filename));
      exit(1);
    }
    else if (output == "topology")
    {
      cout<<MC.T.write(false)<<endl;
      exit(1);
    }
    else if (output == "tree")
    {
      cout<<MC.T.write(true)<<endl;
      exit(1);
    }
    else if (output == "mtree")
    {
      cout<<MC.T.write(false)<<endl;
      for(int i=0;i<MC.branch_order.size();i++) {
	const Partition& P = MC.partitions[MC.branch_order[i]];
	if (not P.full())
	  cout<<P<<endl;
      }
      exit(1);
    }
    else if (output == "lengths")
    {
      for(int i=0;i<MC.branch_order.size();i++) {
	int b = MC.branch_order[i];
	const Partition& P = MC.partitions[b];
	cout<<"branch "<<MC.branch_length(b)<<"\n";
	cout<<P<<endl;
      }
      for(int n=0;n<MC.n_nodes();n++) {
	if (MC.node_length(n) > 0) {
	  cout<<"node "<<MC.node_length(n)<<"\n";
	  cout<<MC.partitions[MC.branch_to_node(n)]<<endl;
	}
      }
      exit(1);
    }



    // lay out using circular tree layout
    if (args["layout"].as<string>() == "equal-angle") 
    {
      tree_layout L = equal_angle_layout(MC);
      L.rotate_for_aspect_ratio(xw,yw);
      tree_plotter tp(L, xw, yw, font_size);
      if (args["labels"].as<string>() != "horizontal")
	tp.horizontal_names = false;

      string filename = name+"-tree";
      if (args.count("out"))
	filename = args["out"].as<string>();

      draw(filename,output,tp);
      exit(0);
    }
    else if (args["layout"].as<string>() == "equal-daylight")
    {
      bool greedy = (args.count("greedy") > 0);
      bool no_shade = (args.count("no-shade") > 0);
      tree_layout L = equal_daylight_layout(MC,greedy,no_shade);
      L.rotate_for_aspect_ratio(xw,yw);
      tree_plotter tp(L, xw, yw, font_size);
      if (args["labels"].as<string>() != "horizontal")
	tp.horizontal_names = false;

      string filename = name+"-tree";
      if (args.count("out"))
	filename = args["out"].as<string>();

      draw(filename,output,tp);	
      exit(0);
    }
    else if (args["layout"].as<string>() == "fr")
    {
      graph_layout L = layout_on_circle(MC,2);
      L = fruchterman_reingold_layout(L,xw,yw);

      string filename = name+"-mctree";
      if (args.count("out"))
	filename = args["out"].as<string>();

      graph_plotter gp(L, xw, yw, font_size);
      draw(filename,output,gp);
      exit(0);
    }
    
    
    // lay out as a graph
    MC.scale(1.0/MC.total_length());
    graph_layout L2 = layout_on_circle(MC,2);
    
    if (args.count("tree-layout-initial"))
    {
      tree_layout LA = equal_angle_layout(MC);
      //cout<<"equal angle edge error = "<<LA.edge_length_error()<<endl;
      // *Transfer tree layout to graph layout* -- this only works when the graph IS a tree
      // otherwise, multiple graph nodes would end up at the same position
      for(int i=0;i<MC.C;i++)
	L2.node_positions[i] = LA.node_positions[MC.graph_node_to_tree_node(i)];
    }

    graph_layout L3 = L2;


    double length_weight = 10000000;

    int n_iterations = args["iterations"].as<int>();
    /*
    for(int i=0;i<10;i++) {
      double w = length_weight/100*double(i)/10;
      L3 = energy_layout(L3,energy2(1,w,1,0),1000);
    }
    */

    double stretchy_weight = 1.0;
    if (args.count("draw-clouds") and args["draw-clouds"].as<string>() == "only") {
      stretchy_weight = 0.1;

    }


    // FIXME - the repulsion should actually depend on the number of nodes!
    if (not args.count("tree-layout-initial")) {
      energy_layout(L3,energy2(1,1000,stretchy_weight,0),400);
      energy_layout(L3,energy2(1,10000,stretchy_weight,0),400);
      energy_layout(L3,energy2(1,100000,stretchy_weight,0),400);
      energy_layout(L3,energy2(1,1000000,stretchy_weight,0),800);
    }

    for(int i=0;i<n_iterations;i++) 
    {
      energy_layout(L3,energy2(1,length_weight,stretchy_weight,0));
      L3.rotate_for_aspect_ratio(xw,yw);

      graph_plotter gp(L3, xw, yw, font_size);

      if (args.count("draw-clouds")) {
	gp.draw_clouds = true;
	if (args["draw-clouds"].as<string>() == "only")
	  gp.draw_type_2_edges = false;
      }
      
      string filename = name+"-mctree";
      if (args.count("out"))
	filename = args["out"].as<string>();
      
      draw(filename,output,gp);
    }

    // improve angular resolution
    int a_iterations = args["angle_iterations"].as<int>();
    for(int i=0;i<a_iterations;i++) {
      energy_layout(L3,energy2(1,10000000,0.01,50));
      L3.rotate_for_aspect_ratio(xw,yw);
      graph_plotter gp(L3, xw, yw, font_size);

      if (args.count("draw-clouds")) {
	gp.draw_clouds = true;

	if (args["draw-clouds"].as<string>() == "only")
	  gp.draw_type_2_edges = false;
      }

      string filename = name+"-mctree";
      if (args.count("out"))
	filename = args["out"].as<string>();

      draw(filename,output,gp);
    }
  }
  catch (std::exception& e) {
    cerr<<"draw-tree: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}


/*

Drawing cloud/range representations.

1. We have a collection of ranges R[i].
2. Some branches connect TO THE RANGES instead of to nodes.
3. All ranges that share a node must have a different color.
4. Ranges must be ordered -> this ordering is used in drawing to determine
                             which ranges are drawn on top of other ranges
			     at nodes and on edges.
   (a) Q:Do we need to have (R[i] in R[j] -> R[i] < R[j])?
       A?: Well... if 

Alternatives for drawing:
(a) At nodes, clouds may have changed widths in order to not obscure each other.
(b)  ... OR they could be transparent to some degree. (widths from branches)
(c)  ....OR they could just obscure each other. (widths from branches)

(a) We could also just draw branches as multiple adjacent/parallel lines to indicate
    the ranges that include each branch. 
    - black -> no ranges
    - red -> green

    Thus, the drawing has no nesting of ranges.

(b) Cloud representation:
    - allows (?requires) nesting of ranges.

Alternatives for layout:
(a) Resolve to MF tree. (Specifies attatchment points at existing nodes)
(b) Resolve to MF tree, but allow attachment points in the middle of branches.
    (b1) Require the new branch-pieces to be parallel.

 */
