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

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <string>
#include <iostream>
#include <vector>
#include <fstream>
#include <memory>
#include <optional>
#include <tuple>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// FIXME - Actually draw arrows on type 2 edges.
//       - Figure out how to use boost's Kamada-Kawai layout.

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

#include "util/assert.hh"
#include "util/myexception.hh"
#include "tree/sequencetree.hh"
#include "mctree.hh"
#include "util/mapping.hh"
#include "util/string/join.hh"
#include "util/string/convert.hh"
#include "util/string/split.hh"
#include "util/permute.hh"
#include "util/rng.hh"
#include "util/io.hh"
#include "util/set.hh"
#include "util/wrap.hh"
#include "util/range.hh"

extern int log_verbose;

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

// cairo and/or fontconfig leak memory when drawing text.
extern "C"
const char* __asan_default_options() { return "detect_leaks=0"; }

// Plotters draw in tree units with a common scale to points. Final page sizing happens after
// recording, so neither labels nor cloud clipping need to know an output surface's boundaries.
class cairo_plotter
{
    double scale_;
    double font_size_;
public:
    virtual void operator()(cairo_t*) = 0;
    double scale() const {return scale_;}
    double font_size() const {return font_size_;}

    cairo_plotter(double scale, double fs): scale_(scale), font_size_(fs) {}
    virtual ~cairo_plotter() = default;
};

variables_map parse_cmd_line(int argc,char* argv[])
{ 
    using namespace po;

    // named options
    options_description input("Input options");
    input.add_options()
	("help,h", "Produce help message")
	("width,W",value<double>(),"Output width in inches; otherwise fit the drawing")
	("height,H",value<double>(),"Output height in inches; otherwise fit the drawing")
	("file",value<string>(),"predicates to examine")
	("output",value<string>()->default_value("pdf"),"Type of output to write: tree, topology, mtree, lengths, dot, ps, pdf, svg")
	("out",value<string>(),"Output filename (without extension)")
	("full","Consider only full splits by collapsing any partial splits.")
	("iterations",value<int>()->default_value(2),"Graph optimization rounds after initialization; draw once after both stages")
	("font-size",value<double>()->default_value(10),"Font size in points before explicit output resizing")
	("angle_iterations",value<int>()->default_value(0),"Additional graph optimization rounds with small-angle penalties")
	("labels",value<string>()->default_value("horizontal"),"Are the names horizontal or angled?")
	("collapse","Give node lengths evenly to neighboring branches and zero the node lengths.")
	("layout",value<string>()->default_value("graph"),"Layout method: graph, equal-angle, equal-daylight")
	("greedy","For equal-daylight layout: redistribute all angular gaps equally at each node")
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
	cout<<"Draw NEWICK (and some other) formatted files.\n\n";
	cout<<"Usage: draw-tree [OPTIONS] <tree file>\n\n";
	cout<<all<<"\n";
	exit(0);
    }

    if (not args.count("file"))
	throw myexception()<<"No file supplied.";

    if (args.count("verbose")) log_verbose = 1;

    return args;
}


/// FIXME - name collision!
// Biological lengths: branch_lengths stores both directed orientations, while T stores the
// corresponding full-tree lengths. Loading, scaling, and collapsing must keep these copies in
// sync. Node lengths describe unresolved length; converting them to drawing radii is separate.
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

    MC_tree_with_lengths(const PTree& tree)
	:MC_tree(tree),
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
    vector<partition>  branches;
    vector<double> branch_lengths;
  
    vector<partition> nodes;
    vector<double> node_lengths;

    //-------------------- Read lengths from file --------------------//
    checked_ifstream file(filename,"multi-connected tree file");

    string line;

    bool with_lengths = true;
    bool first_line = true;

    vector<string> names;
    while(file) 
    {
	while (portable_getline(file,line) and not line.size()) {}
	if (not line.size()) break;

	if (first_line and line[0] == '(') 
	{
	    with_lengths=false;
	    SequenceTree T = standardized(line);
	    names = T.get_leaf_labels();
            for(int b=0;b<T.n_branches();b++)
            {
                partition P = partition_from_branch(T,b);
                double L = 1.0;
                if (T.branch(b).has_length())
                {
                    L = T.branch(b).length();
                    if (L < 0)
                        L = 1;
                    else
                        with_lengths = true;
                }
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
	    if (not file or not portable_getline(file,line))
		throw myexception()<<"Missing partition after 'branch'!";

	    if (not line.size())
		throw myexception()<<"Empty line after 'branch'!";

	    double length = convertTo<double>(words[1]);
	    Partition P(line);
	    if (names.empty()) names = P.names;

	    branches.push_back(P);
	    branch_lengths.push_back(length);
	}
	else if (words.size() == 2 and words[0] == "node")
	{
	    // get the next line
	    if (not file or not portable_getline(file,line))
		throw myexception()<<"Missing partition after 'node'!";

	    if (not line.size())
		throw myexception()<<"Empty line after 'node'!";

	    double length = convertTo<double>(words[1]);
	    Partition P(line);
	    if (names.empty()) names = P.names;

	    assert(not std::isnan(length));

	    nodes.push_back(P);
	    node_lengths.push_back(length);
	}
	else {
	    if (with_lengths or line.find('|') == string::npos)
		throw myexception()<<"Don't understand line='"<<line<<"'";
	    Partition P(line);
	    if (names.empty()) names = P.names;
	    branches.push_back(P);
	    branch_lengths.push_back(1);
	}
    }
    
    if (log_verbose) cerr<<"draw-tree: Read "<<branches.size()<<" partitions"<<endl;

    //--------------- Construct MC tree with lengths  ---------------//
    const auto retained = check_MC_partitions(names, branches).partitions;
    MC_tree_with_lengths MC({names, retained});

    // Filtering can remove entries from anywhere in the list. Match surviving partitions to
    // their original lengths; duplicate partitions retain the first entry, as the filter does.
    for(const auto& P: retained)
    {
        auto original = find_index(branches, P);
        assert(original);
        double L = branch_lengths[*original];

	auto b1 = find_index(MC.partitions,P);
	if (not b1)
	    throw myexception()<<"Can't find partition I just added to tree!\n"<<"     "<<Partition(names, P);

	int b2 = MC.reverse(*b1);

	MC.branch_length(*b1) = MC.branch_length(b2) = L;

	if (P.full())
	    MC.T.directed_branch(*b1).set_length(L);
    }

    for(int i=0;i<nodes.size();i++)
    {
	double L = node_lengths[i];

	int b = MC.find_branch(nodes[i]);

	if (b == -1)
	    throw myexception()<<"Can't find node in tree!\n"<<"    "<<Partition(names, nodes[i]);

	int n = MC.mapping[b];
	int d = MC.degree(n);

        if (d <= 3)
        {
            if (log_verbose)
            {
                cerr<<"draw-tree: Processing node '"<<Partition(MC.names(),nodes[i])<<"'"<<endl;
                cerr<<"draw-tree: Found as branch '"<<Partition(MC.names(),MC.partitions[b])<<"'"<<endl;
                cerr<<"draw-tree: degree = "<<d<<endl;
                for(int j=0;j<2*MC.n_branches();j++)
                    if (MC.mapping[j] == n)
                        cerr<<"draw-tree: "<<Partition(MC.names(), MC.partitions[j])<<endl;
            }
            throw myexception()<<"Node length given for node of degree "<<d<<"; expected degree at least 4.";
        }

	MC.node_length(n) = L;
    }

    return MC;
}

struct point_position
{
    double x;
    double y;
    point_position():x(0),y(0) {}
    point_position(double x, double y):x(x),y(y) {}
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

    return cross;
}

// Unused geometric primitive: test intersection with extended segment-parameter intervals.
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

    return cross;
}

// Unused geometric primitive: test the forward ray m1→m2 against segment n1→n2.
bool ray_points_to_segment(const point_position& m1,
			   const point_position& m2,
			   const point_position& n1,
			   const point_position& n2)
{
    double t,s;
    if (not lines_cross(m1,m2,n1,n2,t,s)) return false;

    bool cross = ((0 <= s) and (s <= 1) and (0 <= t));

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

// Positions and radii use tree units. Renderers convert these to points; page arguments are
// inches. Fonts and strokes start at fixed point sizes, before any final output resizing.
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
    // A layout copy: branch lengths include both endpoint radii (center-to-center distances),
    // unlike the biological lengths in MC_tree_with_lengths::T. Node indices belong to this T.
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


// Retained diagnostic: compare every pair of edges in two directed subtrees.
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


int find_directed_branch(const Tree& T,const partition& p)
{
    for(int b=0; b<2*T.n_branches(); b++)
	if (branch_partition(T,b) == p.group2)
	    return b;
    return -1;
}

// Traverse each edge once to obtain topological (not branch-length) distances from a node.
vector<int> edge_distances(const Tree& T, int start)
{
    vector<int> distances(T.n_nodes(), -1);
    vector<int> pending{start};
    distances[start] = 0;
    for(size_t i=0;i<pending.size();i++)
        for(auto branch = T.node(pending[i]).branches_out(); branch; branch++)
        {
            int target = (*branch).target();
            if (distances[target] != -1) continue;
            distances[target] = distances[pending[i]] + 1;
            pending.push_back(target);
        }
    return distances;
}

int descendant_tip_count(const Tree& T,int b)
{
    return branch_partition(T,b).count();
}


// Allocate the same tip-count angular widths while alternating the smaller child's side
// along unequal binary continuations. Only drawing order changes, not the tree's topology.
void equal_angle_layout(tree_layout& L, int parent, double min_a, double max_a, bool smaller_last)
{
    SequenceTree& T = L.T;
    if (T.directed_branch(parent).target().is_leaf_node()) return;

    vector<int> children;
    for (out_edges_iterator b = T.directed_branch(parent).branches_after(); b; b++)
        children.push_back(*b);
    int continuing = -1;
    if (children.size() == 2)
    {
        int n0 = descendant_tip_count(T, children[0]);
        int n1 = descendant_tip_count(T, children[1]);
        if (n0 != n1)
        {
            continuing = children[n0 > n1 ? 0 : 1];
            if (n0 > n1) std::swap(children[0], children[1]);
            if (smaller_last) std::swap(children[0], children[1]);
        }
    }

    double a1 = min_a;
    for (int b: children)
    {
        double a2 = a1 + (max_a-min_a)*descendant_tip_count(T,b)/descendant_tip_count(T,parent);
        double a = (a1+a2)/2.0;
        auto branch = T.directed_branch(b);
        int n1 = branch.source();
        int n2 = branch.target();
        double length = branch.length();
        L.node_positions[n2] = L.node_positions[n1];
        L.node_positions[n2].x += length*cos(a);
        L.node_positions[n2].y += length*sin(a);

        // Switching sides reverses the larger child's turn by half the smaller angular width.
        // Carry alternation down that child, so equal-tip caterpillar turns cancel in pairs.
        // Other subtrees restart independently; ties/multifurcations retain iterator order.
        // A unary node makes no ordering choice, so it passes the state through unchanged.
        bool next_side = children.size() == 1 ? smaller_last : (b == continuing and not smaller_last);
        equal_angle_layout(L, b, a1, a2, next_side);
        a1 = a2;
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
    // Subdivision experiment: insert intermediate nodes on branches while conserving length,
    // then remap the original radii. Retained as code to document the required index remapping.
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

    // In a tree, a farthest node from any start is a diameter endpoint. For diameter
    // endpoints a,b, max(d(n,a),d(n,b)) is the maximum distance from n to any node (and
    // hence to any tip). Minimizing this gives the same center as the old exhaustive
    // tip-distance search. Strict improvement in index order retains its lowest-index tie.
    auto distances = edge_distances(MF,0);
    int a = std::max_element(distances.begin(), distances.end()) - distances.begin();
    auto from_a = edge_distances(MF,a);
    int b = std::max_element(from_a.begin(), from_a.end()) - from_a.begin();
    auto from_b = edge_distances(MF,b);
    int root = 0;
    for(int n=1;n<MF.n_nodes();n++)
        if (std::max(from_a[n],from_b[n]) < std::max(from_a[root],from_b[root]))
            root = n;

    tree_layout L(MF);
    L.node_radius = node_radius;

    double a1 = 0;
    for(out_edges_iterator b = MF.node(root).branches_out();b;b++) 
    {
	double a2 = a1 + 2.0 * M_PI * descendant_tip_count(MF,*b)/MF.n_leaves();
	double a = (a1+a2)/2.0;
    
	// cerr<<"branch "<<(*b).name()<<"  descendant_tip_count = "<<descendant_tip_count(MF,*b)<<endl;

	int n1 = (*b).source();
	int n2 = (*b).target();
	double length = (*b).length();

	L.node_positions[n2] = L.node_positions[n1];
	L.node_positions[n2].x += length*cos(a);
	L.node_positions[n2].y += length*sin(a);

	// cerr<<"edge target = "<<n2<<"   a1 = "<<a1*180/M_PI<<" a = "<<a*360/(2*M_PI)<<" a2 = "<<a2*180/M_PI<<"   length = "<<length<<"  x = "<<    L.node_positions[n2].x<<" y = "<<L.node_positions[n2].x<<endl;
	equal_angle_layout(L,*b,a1,a2,false);
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

// A connected arc in increasing atan2 angle (clockwise with Cairo's downward y axis).
// Endpoints wrap into [0,2π); min/max denote start/end, not numerical extrema. Equal endpoints
// describe a point, while full_circle represents the entire circle. A default range is unset
// until initialized or made full. Extension forms one covering arc, not a disjoint set union.
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

    double measure() const
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

    const circular_range& extend_to_cover(const circular_range& r);
  
    double gap_from(const circular_range& preceding) const;
};

ostream& operator<<(ostream& o, const circular_range& R)
{
    if (R.is_full_circle())
	o<<"(full_circle)";
    else
	o<<"("<<R.min()<<", "<<R.max()<<")";
    return o;
}

bool overlaps(const circular_range& r1, const circular_range& r2)
{
    if (r1.is_full_circle() or r2.is_full_circle()) return true;
    if (r1.contains(r2.min()) or r1.contains(r2.max())) return true;
    if (r2.contains(r1.max())) return true;
    return false;
}

circular_range full_circle_range()
{
    circular_range R;
    R.set_full_circle();
    return R;
}

// Directed gap from the preceding arc's end to this arc's start; covered ends give zero.
double circular_range::gap_from(const circular_range& r) const
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

// Extend the current arc to cover r. Disjoint arcs use the shorter extension; an exact tie
// extends the start toward r. Preserve these sequential choices when accumulating many arcs.
const circular_range& circular_range::extend_to_cover(const circular_range& r)
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
  
    return *this;
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

        // Include the complete line segment's angular extent, not only its endpoints.
        R.extend_to_cover(A2);
    }

    // Each target belongs to this component, including the first branch's target; the pivot
    // does not. Accumulate circles after segments to preserve the existing arc-extension order.
    // Tangents to a disk at distance d give half-width asin(r/d). A pivot inside the disk sees
    // every direction; at d == r the disk occupies a semicircle.
    for (const auto& branch: branches)
    {
        int target = branch.target();
        double radius = L.node_radius[target];
        if (radius == 0) continue;
        const auto& p = L.node_positions[target];
        double distance = std::hypot(p.x-c.x, p.y-c.y);
        if (distance < radius)
        {
            R.set_full_circle();
            break;
        }
        double angle = std::atan2(p.y-c.y, p.x-c.x);
        double half_width = std::asin(std::min(1.0, radius/distance));
        R.extend_to_cover(circular_range(angle-half_width, angle+half_width));
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

// Balance gaps around each component and return the largest elementary rotation.
double equalize_daylight(tree_layout& L,int n)
{
    double movement = 0;
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
        // Only component b moves in this step. Its two minima are the same entries formerly
        // obtained from all pairs; recompute them after each rotation to retain update order.
        double daylight_before = 2*M_PI;
        double daylight_after = 2*M_PI;
        for(int j=0;j<branches.size();j++)
            if (j != b)
            {
                daylight_after = std::min(daylight_after, angles[j].gap_from(angles[b]));
                daylight_before = std::min(daylight_before, angles[b].gap_from(angles[j]));
            }

        if (daylight_after > 0 and daylight_before > 0)
        {
            double delta = (daylight_after - daylight_before)/2.0;
	    angles[b].shift(delta);
	    L.rotate_subtree(branches[b],delta);
            // Equivalent turns describe the same position; measure the shorter angular displacement.
            movement = std::max(movement, std::abs(std::remainder(delta, 2*M_PI)));
	}
    }
    return movement;
}

// Redistribute available daylight equally and return the largest elementary rotation.
double equalize_daylight_greedy(tree_layout& L,int n)
{
    double movement = 0;
    const Tree& T = L.T;

    // get outwards branches - which point to subtrees
    vector<int> branches;
    for(const_out_edges_iterator i = T.node(n).branches_out();i;i++) {
	branches.push_back(*i);
    }
  

    // The initializer can reorder children without changing topology iterator order. Sort by
    // current direction, but retain the old first branch as the fixed reference. Coincident
    // directions use branch index; zero-length attachments retain get_angle's convention.
    int reference = branches.front();
    std::sort(branches.begin(), branches.end(), [&](int a, int b) {
        return std::pair(get_angle(L,a), a) < std::pair(get_angle(L,b), b);
    });
    std::rotate(branches.begin(), std::find(branches.begin(), branches.end(), reference), branches.end());

    //get the circular ranges for branch subtree
    vector<circular_range> angles;
    double total_occlusion = 0;
    for(int i=0;i<branches.size();i++)  {
	angles.push_back( get_angles(L,branches[i],n) );
	total_occlusion += angles.back().measure();
    }

    // Pack rigid covering arcs in geometric cyclic order, leaving g=(2pi-sum(widths))/k
    // between each pair. The final wrap gap is also g because widths+k*g=2pi. Initial arcs
    // may overlap: summed widths describe space required after packing, not initial occupancy.
    // Each nonzero attachment lies in its arc, so positive final gaps preserve cyclic order.
    // Evaluate crossings only after the complete pivot update; intermediate arcs may overlap.
    if (total_occlusion < 2*M_PI)
    {
	double daylight_per_subtree = (2*M_PI - total_occlusion)/branches.size();

	for(int b=1; b<branches.size(); b++) 
	{
	    double theta = circular_minus(angles[b].min(), angles[b-1].max());
	    double delta = std::remainder(daylight_per_subtree - theta, 2*M_PI);

	    if (delta != 0) {
		L.rotate_subtree(branches[b], delta);
                // The applied rotation is already the shorter equivalent turn.
                movement = std::max(movement, std::abs(delta));
		angles[b].shift(delta);
	    }
	}
    }

    if (L.edges_cross()) throw myexception()<<"equalize_daylight(L,n): edges cross - should not happen!";
    return movement;
}

// the size of a split is the size of its smallest set.
// the size of a node (a multi-partition) is the size of its largest partition.
// this will be the size of the second-largest set.
int node_size(const Tree& T,int n)
{
    if (n < 2) std::abort();

    vector<int> S;
    for(const_out_edges_iterator i = T.node(n).branches_out();i;i++)
	S.push_back(descendant_tip_count(T,*i));

    std::sort(S.begin(),S.end());

    return S[S.size()-2];
}

vector<int> node_order(const Tree& T)
{
    vector<int> nodes;
    for(int i=T.n_leaves();i<T.n_nodes();i++) 
	nodes.push_back(i);

    // The comparator indexes scores by node number; only internal nodes have a node_size.
    vector<int> S(T.n_nodes(),0);
    for(int n: nodes)
	S[n] = node_size(T,n);

    std::sort(nodes.begin(), nodes.end(), sequence_order<int>(S));
    std::reverse(nodes.begin(), nodes.end());

    return nodes;
}

// Try each pivot in the topology-dependent order. Only positions change, so rejected moves
// need a coordinate snapshot, not a copy of the tree and all its cached partitions.
// Return the largest accepted movement; a rejected rotation contributes zero.
double equalize_daylight(tree_layout& L, const vector<int>& nodes, bool greedy, bool no_shade)
{
    double movement = 0;
    for(int node: nodes)
    {
        vector<point_position> previous_positions;
        if (no_shade) previous_positions = L.node_positions;
        double delta = greedy ? equalize_daylight_greedy(L,node) : equalize_daylight(L,node);
        if (no_shade and shades(L))
            L.node_positions = std::move(previous_positions);
        else
            movement = std::max(movement, delta);
    }
    return movement;
}

// Equal-angle initialization converts biological lengths to center distances exactly once.
// Reuse topology-dependent pivot order until accepted movements are small, or reach the cap.
tree_layout equal_daylight_layout(const MC_tree_with_lengths& MC, bool greedy=false, bool no_shade=false)
{
    tree_layout L = equal_angle_layout(MC);
    const auto nodes = node_order(L.T);
    constexpr double tolerance = 0.001; // radians, measured per elementary rotation
    constexpr int max_sweeps = 50;
    int sweeps = 0;
    double movement;
    do
    {
        movement = equalize_daylight(L, nodes, greedy, no_shade);
        ++sweeps;
    } while (movement >= tolerance and sweeps < max_sweeps);
    if (log_verbose)
        cerr << "draw-tree: Equal-daylight stopped after " << sweeps << " sweeps: "
             << (movement < tolerance ? "movement below 0.001 radians" : "50-sweep limit reached") << ".\n";
    return L;
}

// Retained geometric traversal experiment: follow the next outgoing direction around the drawing.
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



// Draw node bubbles after branches, using tree-unit radii and point-scaled dashed outlines.
void draw_node_circles(cairo_t* cr, const common_layout& layout, double line_width)
{
    cairo_save(cr);
    const double dashes[] = {3*line_width, 3*line_width};
    cairo_set_dash(cr, dashes, 2, 0);
    cairo_set_line_width(cr, line_width/2);
    for(int n=0;n<layout.node_radius.size();n++)
        if (layout.node_radius[n] > 0)
        {
            const auto& p = layout.node_positions[n];
            const auto& color = layout.node_color[n];
            cairo_arc(cr, p.x, p.y, layout.node_radius[n], 0, 2*M_PI);
            cairo_set_source_rgb(cr, color.R, color.G, color.B);
            cairo_fill_preserve(cr);
            cairo_set_source_rgb(cr, 0, 0, 0);
            cairo_stroke(cr);
        }
    cairo_restore(cr);
}

namespace {

// Keep measured text and its anchor together through candidate selection and painting.
struct tip_label
{
    string name;
    point_position tip, direction, baseline;
    double radius;
    cairo_text_extents_t extents;
};

// Measure the preferred position, retaining the existing branch-direction offsets.
tip_label measure_tip_label(cairo_t* cr, const string& name, const point_position& tip,
                            const point_position& neighbor, double radius,
                            const point_position& center, double line_width)
{
    point_position direction(tip.x-neighbor.x, tip.y-neighbor.y);
    double distance = std::hypot(direction.x, direction.y);
    if (distance == 0)
    {
        direction = point_position(tip.x-center.x, tip.y-center.y);
        distance = std::hypot(direction.x, direction.y);
    }
    if (distance == 0) direction = point_position(1, 0);
    else { direction.x /= distance; direction.y /= distance; }
    cairo_text_extents_t extents;
    cairo_text_extents(cr, name.c_str(), &extents);
    double x = tip.x + direction.x*(line_width+extents.height/8);
    double y = tip.y + direction.y*(line_width+extents.height/2) + extents.height/2;
    if (direction.x < 0) x -= extents.width;
    return {name, tip, direction, point_position(x, y), radius, extents};
}

// Cairo bearings locate the ink relative to the baseline; padding reserves physical clearance.
cairo_rectangle_t label_rectangle(const tip_label& label, double padding)
{
    const auto& e = label.extents;
    return {label.baseline.x + e.x_bearing-padding, label.baseline.y + e.y_bearing-padding,
            e.width+2*padding, e.height+2*padding};
}

// Include boundary contact when testing the reserved label clearance.
bool rectangles_intersect(const cairo_rectangle_t& a, const cairo_rectangle_t& b)
{
    return a.x <= b.x+b.width and b.x <= a.x+a.width and
           a.y <= b.y+b.height and b.y <= a.y+a.height;
}

// Clip the segment's parameter interval against both rectangle slabs, including contact.
bool rectangle_intersects_segment(const cairo_rectangle_t& r, point_position a, point_position b)
{
    double first = 0, last = 1;
    for (int axis = 0; axis < 2; ++axis)
    {
        double start = axis ? a.y : a.x;
        double delta = axis ? b.y-a.y : b.x-a.x;
        double lo = axis ? r.y : r.x;
        double hi = lo + (axis ? r.height : r.width);
        if (delta == 0)
        {
            if (start < lo or start > hi) return false;
        }
        else
        {
            double t1 = (lo-start)/delta, t2 = (hi-start)/delta;
            if (t1 > t2) std::swap(t1, t2);
            first = std::max(first, t1);
            last = std::min(last, t2);
            if (first > last) return false;
        }
    }
    return true;
}

// Test the closest point of the rectangle against the filled circle and its outline.
bool rectangle_intersects_circle(const cairo_rectangle_t& r, point_position center, double radius)
{
    double x = std::clamp(center.x, r.x, r.x+r.width);
    double y = std::clamp(center.y, r.y, r.y+r.height);
    return std::hypot(x-center.x, y-center.y) <= radius;
}

// Test padded text against branch strokes and bubbles; cloud interiors are not obstacles.
bool label_intersects_tree(const cairo_rectangle_t& box, const common_layout& layout,
                           const vector<std::pair<point_position,point_position>>& edges, double line_width)
{
    cairo_rectangle_t stroke_box{box.x-line_width/2, box.y-line_width/2,
                                box.width+line_width, box.height+line_width};
    for (const auto& [a,b]: edges)
        if (rectangle_intersects_segment(stroke_box, a, b)) return true;
    for (int n=0; n<layout.node_positions.size(); ++n)
        if (layout.node_radius[n] > 0 and
            rectangle_intersects_circle(box, layout.node_positions[n], layout.node_radius[n]+line_width/4))
            return true;
    return false;
}

// The farthest corner bounds the distance of every point in an axis-aligned rectangle.
double rectangle_radius(const cairo_rectangle_t& box, const point_position& origin)
{
    double dx = std::max(std::abs(box.x-origin.x), std::abs(box.x+box.width-origin.x));
    double dy = std::max(std::abs(box.y-origin.y), std::abs(box.y+box.height-origin.y));
    return std::hypot(dx, dy);
}

// Construct the shortened leader once so placement scores exactly the segment that is drawn.
std::optional<std::pair<point_position,point_position>> label_connector(
    const tip_label& label, const cairo_rectangle_t& box, double pt)
{
    if (label.extents.width == 0 or label.extents.height == 0) return {};
    auto original = label_rectangle(label, pt);
    if (box.x == original.x and box.y == original.y) return {};
    double x = std::clamp(label.tip.x, box.x+pt, box.x+box.width-pt);
    double y = std::clamp(label.tip.y, box.y+pt, box.y+box.height-pt);
    double distance = std::hypot(x-label.tip.x, y-label.tip.y);
    if (distance <= label.radius+2*pt) return {};
    double dx = (x-label.tip.x)/distance, dy = (y-label.tip.y)/distance;
    return std::pair(point_position(label.tip.x+(label.radius+pt)*dx, label.tip.y+(label.radius+pt)*dy),
                     point_position(x-pt*dx, y-pt*dy));
}

// Count branch contacts along the drawn leader, including collinear overlap. Keep these
// semantics local: the existing tree-layout crossing test deliberately ignores parallel lines.
int connector_crossings(const tip_label& label, const cairo_rectangle_t& box,
                       const vector<std::pair<point_position,point_position>>& edges, double pt)
{
    auto connector = label_connector(label, box, pt);
    if (not connector) return 0;
    const auto& [a,b] = *connector;
    int count = 0;
    for (const auto& [c,d]: edges)
    {
        if (c.x == d.x and c.y == d.y) continue;
        double t, s;
        if (lines_cross(a, b, c, d, t, s))
            count += (0 <= t and t <= 1 and 0 <= s and s <= 1);
        else if ((b.x-a.x)*(c.y-a.y) == (b.y-a.y)*(c.x-a.x))
        {
            // Parallel lines containing c are collinear. Projection onto the leader's larger
            // coordinate reduces their intersection to overlap of two closed intervals.
            bool use_x = std::abs(b.x-a.x) >= std::abs(b.y-a.y);
            double a1 = use_x ? a.x : a.y, b1 = use_x ? b.x : b.y;
            double c1 = use_x ? c.x : c.y, d1 = use_x ? d.x : d.y;
            count += std::max(std::min(a1,b1), std::min(c1,d1)) <=
                     std::min(std::max(a1,b1), std::max(c1,d1));
        }
    }
    return count;
}

// Select horizontal rectangles without moving the tree. Three candidates per label form a
// conflict graph, admitting only clear connectors; removing candidates updates conflict degrees.
// Minimum degree preserves choices for other labels; ties prefer the original position, then
// candidate order and leaf index. The nearby-candidate graph has quadratic work/storage.
vector<cairo_rectangle_t> place_horizontal_labels(const vector<tip_label>& labels, const common_layout& layout,
    const vector<std::pair<point_position,point_position>>& edges, double pt)
{
    const double line_width = 2*pt;
    const int count = labels.size();
    vector<cairo_rectangle_t> candidates, placed(count);
    vector<bool> active, assigned(count, false);
    for (int i=0; i<count; ++i)
    {
        const auto& label = labels[i];
        auto box = label_rectangle(label, pt);
        double spacing = label.extents.height + 2*pt;
        bool empty = label.extents.width == 0 or label.extents.height == 0;
        if (empty) { assigned[i] = true; placed[i] = box; }
        for (int choice=0; choice<3; ++choice)
        {
            double offset = choice == 0 ? 0 : (choice == 1 ? spacing : -spacing);
            auto shifted = box;
            shifted.x -= offset*label.direction.y;
            shifted.y += offset*label.direction.x;
            candidates.push_back(shifted);
            active.push_back(not empty and not label_intersects_tree(shifted, layout, edges, line_width) and
                             connector_crossings(label, shifted, edges, pt) == 0);
        }
    }
    vector<vector<int>> conflicts(candidates.size());
    vector<int> degree(candidates.size(), 0);
    for (int i=0; i<candidates.size(); ++i)
        if (active[i])
            for (int j=0; j<i; ++j)
                if (active[j] and i/3 != j/3 and rectangles_intersect(candidates[i], candidates[j]))
                {
                    conflicts[i].push_back(j);
                    conflicts[j].push_back(i);
                    degree[i]++;
                    degree[j]++;
                }
    // Removing a candidate also removes its contribution to every surviving conflict count.
    auto remove = [&](int i) {
        if (not active[i]) return;
        active[i] = false;
        for (int j: conflicts[i])
            if (active[j]) --degree[j];
    };
    while (true)
    {
        int best = -1;
        for (int i=0; i<candidates.size(); ++i)
            if (active[i] and (best == -1 or
                std::tuple(degree[i], i%3, i/3) < std::tuple(degree[best], best%3, best/3)))
                best = i;
        if (best == -1) break;
        int label = best/3;
        placed[label] = candidates[best];
        assigned[label] = true;
        for (int j: conflicts[best]) remove(j);
        for (int j=3*label; j<3*label+3; ++j) remove(j);
    }

    for (int i=0; i<count; ++i)
    {
        if (assigned[i]) continue;
        const auto& label = labels[i];
        // All obstacles lie in a disk of radius R about this tip. Every point of the initial
        // padded label is within B of it. Translating by more than R+B puts the whole label
        // outside that disk (reverse triangle inequality), providing a finite search endpoint.
        double radius = 0;
        for (const auto& [a,b]: edges)
            radius = std::max(radius, std::max(std::hypot(a.x-label.tip.x, a.y-label.tip.y),
                                             std::hypot(b.x-label.tip.x, b.y-label.tip.y)) + line_width);
        for (int n=0; n<layout.node_positions.size(); ++n)
            radius = std::max(radius, std::hypot(layout.node_positions[n].x-label.tip.x,
                                               layout.node_positions[n].y-label.tip.y)
                                     + layout.node_radius[n] + line_width);
        for (int j=0; j<count; ++j)
            if (assigned[j] and labels[j].extents.width > 0 and labels[j].extents.height > 0)
                radius = std::max(radius, rectangle_radius(placed[j], label.tip));
        double initial_radius = 0;
        for (int choice=0; choice<3; ++choice)
            initial_radius = std::max(initial_radius, rectangle_radius(candidates[3*i+choice], label.tip));
        double limit = radius + initial_radius + pt;
        // Search all three parallel rays in increasing one-point displacements. Crossings take
        // priority over distance, with candidate order breaking ties. The escape bound ensures
        // clear text, not a clear connector: retain the fewest crossings if zero is unattainable.
        int best_crossings = -1;
        double distance = 0;
        while (true)
        {
            for (int choice=0; choice<3; ++choice)
            {
                auto box = candidates[3*i+choice];
                box.x += distance*label.direction.x;
                box.y += distance*label.direction.y;
                bool blocked = label_intersects_tree(box, layout, edges, line_width);
                for (int j=0; not blocked and j<count; ++j)
                    if (assigned[j] and j != i and labels[j].extents.width > 0 and labels[j].extents.height > 0)
                        blocked = rectangles_intersect(box, placed[j]);
                if (blocked) continue;
                int crossings = connector_crossings(label, box, edges, pt);
                if (best_crossings == -1 or crossings < best_crossings)
                {
                    placed[i] = box;
                    best_crossings = crossings;
                }
                if (best_crossings == 0) break;
            }
            if (best_crossings == 0 or distance == limit) break;
            distance = std::min(distance+pt, limit);
        }
        assert(best_crossings >= 0);
        assigned[i] = true;
    }
#ifndef NDEBUG
    // The placement pass guarantees clearance for nonempty label bodies, not for leader lines.
    for (int i=0; i<count; ++i)
        if (labels[i].extents.width > 0 and labels[i].extents.height > 0)
        {
            assert(not label_intersects_tree(placed[i], layout, edges, line_width));
            for (int j=0; j<i; ++j)
                if (labels[j].extents.width > 0 and labels[j].extents.height > 0)
                    assert(not rectangles_intersect(placed[i], placed[j]));
        }
#endif
    return placed;
}

// Draw displaced labels' leaders before the text. Leaders are graphical annotations, with
// a thinner dashed gray stroke; placement prefers leaders with fewer branch crossings.
void draw_horizontal_labels(cairo_t* cr, const vector<tip_label>& labels, const common_layout& layout,
    const vector<std::pair<point_position,point_position>>& edges, double pt)
{
    auto placed = place_horizontal_labels(labels, layout, edges, pt);
    cairo_save(cr);
    const double dashes[] = {2*pt, 2*pt};
    cairo_set_dash(cr, dashes, 2, 0);
    cairo_set_line_width(cr, 0.5*pt);
    cairo_set_source_rgb(cr, 0.5, 0.5, 0.5);
    for (int i=0; i<labels.size(); ++i)
    {
        auto connector = label_connector(labels[i], placed[i], pt);
        if (not connector) continue;
        const auto& [a,b] = *connector;
        cairo_move_to(cr, a.x, a.y);
        cairo_line_to(cr, b.x, b.y);
        cairo_stroke(cr);
    }
    cairo_restore(cr);
    for (int i=0; i<labels.size(); ++i)
    {
        cairo_move_to(cr, placed[i].x+pt-labels[i].extents.x_bearing,
                          placed[i].y+pt-labels[i].extents.y_bearing);
        cairo_show_text(cr, labels[i].name.c_str());
    }
}

} // namespace

struct tree_plotter: public cairo_plotter
{
    bool horizontal_names;
    // Drawing is synchronous; the caller owns the layout for the plotter's lifetime.
    const tree_layout& L;
    void operator()(cairo_t*);
    tree_plotter(const tree_layout& tl,double xw, double yw,double fs)
        :cairo_plotter(0.8*std::min(xw*inch/tl.x_width(), yw*inch/tl.y_width()), fs),
         horizontal_names(true), L(tl)
        {}

};

void tree_plotter::operator()(cairo_t* cr)
{
    const double scale = this->scale();
    cairo_scale(cr, scale, scale);

    const double pt = 1.0/scale;
    const double line_width = 2.0*pt;
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

    draw_node_circles(cr, L, line_width);

    cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, font_size()/scale);
  
    if (horizontal_names)
    {
        vector<tip_label> labels;
        point_position center(L.x_center(), L.y_center());
        for (int l=0; l<L.T.n_leaves(); ++l)
        {
            int parent = L.T.directed_branch(l).target();
            labels.push_back(measure_tip_label(cr, L.T.get_label(l), L.node_positions[l],
                L.node_positions[parent], L.node_radius[l], center, line_width));
        }
        vector<std::pair<point_position,point_position>> edges;
        for (int b=0; b<L.T.n_branches(); ++b)
            edges.emplace_back(L.node_positions[L.T.branch(b).source()], L.node_positions[L.T.branch(b).target()]);
        draw_horizontal_labels(cr, labels, L, edges, pt);
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

	    cairo_text_extents_t extents;
            cairo_text_extents(cr, L.T.get_label(l).c_str(), &extents);
            double W = extents.width;
            double H = extents.height;

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

// Coordinates use MC condensed-node indices, not the node indices of MC.T. Graph edges index
// MC.edges; their partition fields identify directed branches. Type 1 edges carry biological
// length, while type 2 edges represent possible wandering attachments.
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

// Energy terms accumulate gradients, not forces; the optimizer moves in the opposite direction.
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

    // Let g be the signed gap between circle boundaries, and h = 0.001 times their combined
    // radius. Use E = C/(g+h) outside and its quadratic continuation at contact inside:
    // E = C/h - C*g/h² + C*g²/h³. The value, slope, and curvature match at g=0, so either
    // branch gives the same contact energy and derivative. Energy decreases as separation
    // increases, including during overlap. Compute that radial derivative before distributing
    // it to the two centers. Point-point pairs retain their existing inverse-distance repulsion.
    double R = GL.node_radius[n1] + GL.node_radius[n2];
    double E, dE_dD;
    if (R == 0)
    {
	double dL = std::max(D,1.0e-15);
	E = C/dL;
	dE_dD = -C/(dL*dL);
    }
    else
    {
	double g = D - R;
	double h = 0.001*R;
	if (g >= 0)
	{
	    E = C/(g+h);
	    dE_dD = -C/((g+h)*(g+h));
	}
	else
	{
	    double u = g/h;
	    E = C/h*(1-u+u*u);
	    dE_dD = -C/(h*h)*(1-2*u);
	}
    }

    if (D < 1.0e-15) D=1.0e-15;
    double temp = -dE_dD/D;

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

    assert(not std::isnan(E));
    return E;
}

double distance_to_line_segment(const graph_layout& GL,int n1,int n2,int n3)
{
    return distance_to_line_segment(GL.node_positions[n1],
				    GL.node_positions[n2],
				    GL.node_positions[n3]);
}



// For noncoincident endpoints a,b, project p onto their segment with t clamped to [0,1].
// With r = p-((1-t)a+tb), E=C|r|². Terms differentiating t vanish: in the interior r is
// perpendicular to b-a; outside, t is constant. At t=0 or 1 these gradients agree with
// the endpoint limits. Unlike differentiating |r| first, this is defined when r is zero.
// TODO: Specify energy/endpoint-gradient handling when a and b coincide (still unsupported).
double graph_energy_function::node_edge_attraction(const graph_layout& GL, vector<point_position>& DEL,
                                                  int n1, int n2, int n3, double C) const
{
    const auto& p = GL.node_positions[n1];
    const auto& a = GL.node_positions[n2];
    const auto& b = GL.node_positions[n3];
    double vx = b.x-a.x, vy = b.y-a.y;
    double t = ((p.x-a.x)*vx + (p.y-a.y)*vy)/(vx*vx + vy*vy);
    t = std::clamp(t, 0.0, 1.0);
    double rx = p.x-(a.x+t*vx), ry = p.y-(a.y+t*vy);
    double gx = 2*C*rx, gy = 2*C*ry;
    DEL[n1].x += gx;
    DEL[n1].y += gy;
    DEL[n2].x -= (1-t)*gx;
    DEL[n2].y -= (1-t)*gy;
    DEL[n3].x -= t*gx;
    DEL[n3].y -= t*gy;
    return C*(rx*rx + ry*ry);
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
    assert(not std::isnan(x21) and std::isfinite(x21));
    assert(not std::isnan(x22) and std::isfinite(x22));
    assert(not std::isnan(D) and std::isfinite(D));
    D = std::max(D,-1.0);
    D = std::min(D,1.0);
    double a = acos(D);
    assert(not std::isnan(a) and std::isfinite(a));

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
    // Restrict this approximation to near-zero angles; other angles need their actual cosine.
    if (std::abs(D-1) < 1.0e-8) D=1.0-1.0e-8;

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

    //  cerr<<"a = "<<a<<"   std::isnan(a) = "<<std::isnan(a)<<"   isnormal(a) = "<<isnormal(a)<<endl;
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

matrix<int> edge_crossing_matrix(const graph_layout& GL)
{
    const unsigned n_edges = GL.MC.edges.size();

    //--- check all edge pairs to see if they cross ---//
    matrix<int> e_cross(n_edges,n_edges);
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
    matrix<int> e_cross = edge_crossing_matrix(GL);

    vector<bool> e_cross_v(e_cross.size1(),false);

    for(int i=0;i < e_cross.size1(); i++)
	for(int j=0;j < e_cross.size2(); j++)
	    if (e_cross(i,j) and GL.MC.edges[i].type == 1 and GL.MC.edges[j].type == 1 ) {
		e_cross_v[i] = true;
		e_cross_v[j] = true;
	    }

    return e_cross_v;  
}

// Retained crossing-penalty experiment: associate crossing edges with nodes in their subgraphs.
matrix<int> crossed_nodes_matrix(const graph_layout& GL)
{
    matrix<int> e_cross = edge_crossing_matrix(GL);

    const MC_tree_with_lengths& MC = GL.MC;

    const unsigned n_edges = MC.edges.size();

    const unsigned n_nodes = MC.n_nodes();

    matrix<int> n_cross(n_nodes,n_nodes);
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
    matrix<int> n_cross = crossed_nodes_matrix(GL);

    vector<bool> n_cross_v(n_cross.size1(),false);

    for(int i=0;i < n_cross.size1(); i++)
	for(int j=0;j < n_cross.size2(); j++)
	    if (n_cross(i,j)) {
		n_cross_v[i] = true;
		n_cross_v[j] = true;
	    }

    return n_cross_v;  
}

       

struct layout_energy: public graph_energy_function
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
    
	    // const unsigned n_edges = MC.edges.size();
	    const unsigned n_nodes = MC.n_nodes();

	    //--- check all edge pairs to see if they cross ---//
	    //    matrix<int> n_cross = crossed_nodes_matrix(GL);

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

		// double L = GL.MC.branch_length(b);
      
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

    layout_energy(double W1, double W2,double W3,double W4)
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


// Independent finite-difference checker for energy gradients; unused in ordinary optimization.
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

// Unfinished alternative layouts, retained as integration sketches for future comparisons.
// Kamada-Kawai still needs an edge-weight property map and an enabled layout invocation.
// The Fruchterman-Reingold experiment below includes its initialization and custom force;
// neither experiment currently supplies a working layout option.
// #include <boost/graph/graph_traits.hpp>
// #include <boost/graph/adjacency_list.hpp>
// #include <boost/graph/topological_sort.hpp>
// #include <boost/graph/kamada_kawai_spring_layout.hpp>
// #include <boost/graph/fruchterman_reingold.hpp>
// 
// typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph; 
// typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
// typedef boost::graph_traits<Graph>::edge_descriptor Edge_t;
// 
// 
// graph_layout kamada_kawai_layout(graph_layout GL)
// {
//     const MC_tree& MC = GL.MC;
//     Graph g(MC.n_nodes());
// 
//     //------------ Construct the Graph --------------//
//     for(int i=0;i<MC.edges.size();i++) {
// 	const mc_tree_edge& e = MC.edges[i];
// 	add_edge(e.from, e.to, g);
//     }
// 
//     //--------- Package position as property ---------//
//     typedef vector<point_position> PositionVec;
//     typedef boost::iterator_property_map<PositionVec::iterator,
//                                          boost::property_map<Graph, boost::vertex_index_t>::type>
// 	PositionMap;
// 
//     PositionMap position(GL.node_positions.begin(), get(boost::vertex_index, g));
// 
//     //--------- Package weights as property ---------//
//     vector<double> weights(num_edges(g));
// 
//     //  typedef vector<double> WeightVec;
//     //  typedef iterator_property_map<WeightVec::iterator, 
//     //    property_map<Graph, edge_index_t>::type>
//     //    WeightMap;
// 
//     //  WeightMap weight(weights.begin(), get(edge_index,g));
// 
//     // kamada_kawai_spring_layout(g, position, weight, boost::side_length(1.0) );
// 
//     return GL;
// }
// 
// struct my_square_distance_attractive_force {
//     template<typename Graph, typename T>
//     T
//     operator()(typename boost::graph_traits<Graph>::edge_descriptor,
// 	       T k,
// 	       T d,
// 	       const Graph&) const
// 	{
// 	    k /= 10;
// 	    d -= k;
// 	    double temp = 100.0*d * d / k;
// 	    if (d < k) temp *= -1;
// 	    return temp;
// 	}
// };
// 
// 
// #include <boost/graph/random_layout.hpp>
// #include <boost/random/linear_congruential.hpp>
// 
// graph_layout fruchterman_reingold_layout(graph_layout GL, double /* width */, double /* height */)
// {
//     /*
//       const MC_tree& MC = GL.MC;
//       Graph g(MC.n_nodes());
// 
//       //------------ Construct the Graph --------------//
//       for(int i=0;i<MC.edges.size();i++) {
//       const mc_tree_edge& e = MC.edges[i];
//       add_edge(e.from, e.to, g);
//       }
// 
//       //--------- Package position as property ---------//
//       typedef vector<point_position> PositionVec;
//       typedef iterator_property_map<PositionVec::iterator,
//       property_map<Graph, vertex_index_t>::type>
//       PositionMap;
// 
//       PositionMap position(GL.node_positions.begin(), get(vertex_index, g));
// 
//       cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;
// 
//       //------------ Initial random layout ------------//
//       minstd_rand gen;
//       gen.seed(rng::get_random_seed());
//       rectangle_topology<> R(-width/2, width/2, -height/2, height/2);
//       random_graph_layout(g, position, R);
//       cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;
// 
//       //------------ Final force-directed layout ------------//
//       //  width *= MC.n_branches()*100;
//       //  height *= MC.n_branches()*100;
//       width *= 100;
//       height *= 100;
// 
//       PositionVec Displacements(num_vertices(g));
//       PositionMap displacements(Displacements.begin(), get(vertex_index, g));
// 
//       fruchterman_reingold_force_directed_layout(g, 
//       position, 
//       R,
//       my_square_distance_attractive_force(),
//       square_distance_repulsive_force(),
//       make_grid_force_pairs(R,position,g),
//       linear_cooling<double>(1000),
//       displacements
//       );
// 
//       cerr<<"x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;
//     */
//     return GL;
// }
// 

void energy_layout(graph_layout& GL, const graph_energy_function& E,int n=1000)
{
    double T = 0;
    int successes=0;

    vector<point_position> gradient;
    //  double E = E(GL,gradient);

    double dt = 1;//E(GL,gradient)/dot(gradient,gradient);

    for(int i=0;i<n;i++) 
    {
	double E1 = E(GL,gradient);
	assert(E1 >= 0);
	if (log_verbose) 
	    cerr<<"iter = "<<i<<" E = "<<E1<<"   T = "<<T<<"   dt = "<<dt<<"   edge_length_error = "<<GL.edge_length_error()<<endl;

	if (log_verbose)
	    cerr<<"  x = "<<GL.xmin()<<" - "<<GL.xmax()<<"      y = "<<GL.ymin()<<" - "<<GL.ymax()<<endl;

	/*
	// check gradient versus D2
	vector<point_position> D2 = energy_derivative_2D(GL,E);
	cerr<<"      derivative error = "<<sqrt(dot(gradient,gradient)/dot(D2,D2))
	<<"      angle = "<<dot(D2,gradient)/sqrt(dot(D2,D2)*dot(gradient,gradient))<<"\n";
	//    gradient = D2;
	*/


	// problem with these equations is STIFFness (? and rotation?)


	// Make a scaled version
	vector<point_position> direction = gradient;
	for(int i=0;i<direction.size();i++) 
	{
	    double v = sqrt(direction[i].x*direction[i].x + direction[i].y*direction[i].y);
	    // A stationary node has zero displacement while other nodes can continue moving.
	    if (v == 0) continue;
	    direction[i].x /= v;
	    direction[i].y /= v;
	}

	vector<point_position> previous_positions = GL.node_positions;
	inc(GL.node_positions,-dt,direction);

	double E2 = E(GL);
	if (E2 > E1) {
	    if (log_verbose) cerr<<"draw-tree:    rejecting  E = "<<E2<<endl;
	    GL.node_positions = previous_positions;
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

            // TODO: Add a convergence rule that distinguishes a settled layout from a tiny
            // step caused by repeated rejection. Absolute positions are not displacements;
            // retain the iteration limit until a suitable criterion has been specified.

	}
    }
}

// Cloud colors use the Welsh-Powell heuristic: join clouds sharing nodes, visit them by
// decreasing neighbor count, and select the smallest color unused by already colored neighbors.
struct cloud 
{
    vector<int> child_nodes;     // Condensed MC-node indices covered by this cloud.

    // Graph-edge indices: MC constructs its type-1 edges in branch_order order. Thus the
    // index i into branch_order is also the edge index; branch_order[i] is a directed branch.
    vector<int> child_edges;

    vector<int> parent_branches; // These DIRECTED branches wander DIRECTLY over this cloud;

    int depth;

    int color = -1;

    vector<int> neighbors;

    int degree() const { return neighbors.size();}

    int size() const {return child_edges.size();}

    cloud():depth(0) {}
    cloud(const vector<int>& v): child_edges(v),depth(0) {}
    cloud(const vector<int>& v,int p): child_edges(v),parent_branches(1,p),depth(0) {}
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
    return overlap(c1.child_edges, c2.child_edges);
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
	vector<int> child_edges;

	// ... to see what (unordered) branches it wanders over.
	for(int i=0;i<MC.branch_order.size();i++)
	{
	    int b2 = MC.branch_order[i];
	    if (MC.directly_wanders_over(b,b2))
		child_edges.push_back(i);
	}

	if (not child_edges.size()) continue;

	//How many branches does this branch wander over?
	//cerr<<"size = "<<child_edges.size()<<endl;

	// Search the clouds found so far... which should be in order of increasing size
	int which = -1;
	int larger = -1;
	for(int i=0;i<clouds.size();i++) {
	    if (clouds[i].child_edges == child_edges)
		which = i;

	    // select the FIRST (and therefore SMALLEST) cloud that is larger than this one.
	    if (clouds[i].size() > child_edges.size() and larger == -1)
		larger = i;
	}

	//The first clouds that is larger than this one
	//cerr<<"larger = "<<larger<<endl;
	//cerr<<endl;

	// NOTE: clouds should be sorted by size.
	if (which == -1)
	{ 
	    cloud C(child_edges,b);

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
	for(int j=0;j<clouds[i].child_edges.size();j++)
	{
	    int b = clouds[i].child_edges[j];

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
    // Drawing is synchronous; the caller owns the layout for the plotter's lifetime.
    const graph_layout& L;
    void operator()(cairo_t*);

    bool draw_clouds = false;

    bool draw_type_2_edges = true;

    graph_plotter(const graph_layout& gl,double xw,double yw, double fs)
	:cairo_plotter(0.8*std::min(xw*inch/gl.x_width(), yw*inch/gl.y_width()), fs),
	 L(gl)
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

    for(int i=0;i<C.child_edges.size();i++) 
    {
	int b = C.child_edges[i];
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

// Bound the stroked branch locally so cloud exclusion does not depend on the page or surface.
// The padding keeps the outer clip edge away from the stroke, including on recording surfaces.
void cairo_add_branch_bounds_as_path(cairo_t* cr, double x1, double y1, double x2, double y2)
{
    cairo_move_to(cr, x1, y1);
    cairo_line_to(cr, x2, y2);
    double left, top, right, bottom;
    cairo_stroke_extents(cr, &left, &top, &right, &bottom);
    double padding = cairo_get_line_width(cr);
    cairo_new_path(cr);
    cairo_rectangle(cr, left-padding, top-padding, right-left+2*padding, bottom-top+2*padding);
}


void graph_plotter::operator()(cairo_t* cr)
{
    // Optional crossing diagnostic: compute edge_crossing_vector(L) and use its entries
    // to color crossed edges in the drawing loop below. O(E²); disabled in normal rendering.

    //  matrix<int> n_cross = crossed_nodes_matrix(L);

    //  vector<bool> n_cross_v = crossed_nodes_vector(L);

    const double scale = this->scale();
    cairo_scale(cr, scale, scale);

    const double pt = 1.0/scale;
    const double line_width = 2.0*pt;
    const double dashes[] = {3.0*line_width, 3.0*line_width};
    cairo_set_line_width(cr, line_width);

    vector<cloud> clouds;
    if (draw_clouds) clouds = get_clouds(L.MC);
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
		cerr<<"draw-tree:     #b : "<<clouds[c].child_edges.size()<<endl;
		cerr<<"draw-tree:     #n : "<<clouds[c].child_nodes.size()<<endl;

		for(int i=0;i<clouds[c].child_nodes.size();i++)
		    cout<<"draw-tree:   node : "<<clouds[c].child_nodes[i]<<endl;
	    }
	    for(int i=0;i<clouds[c].size();i++)
	    {
		int e = clouds[c].child_edges[i];
		if (log_verbose) cerr<<"draw-tree:   edge : "<<e<<"\n";
	
		int n1 = L.MC.edges[e].from;
		int n2 = L.MC.edges[e].to;
		// int t =  L.MC.edges[e].type;
		// int b =  L.MC.edges[e].partition;
	
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

                // Only cloud-only rendering needs to find attachment clouds and clip branches.
                if (draw_clouds and not draw_type_2_edges)
                {
                    int c1 = find_cloud(clouds,b);
                    int c2 = find_cloud(clouds,L.MC.reverse(b));
                    if (c1 != -1 or c2 != -1)
                    {
                        // Retain the existing outer-rectangle/cloud winding convention.
                        // To inspect a cloud outline, stroke cairo_make_cloud_path separately.
                        cairo_add_branch_bounds_as_path(cr, x1, y1, x2, y2);
                        if (c1 != -1) cairo_make_cloud_path(cr, clouds[c1], line_width,L);
                        if (c2 != -1) cairo_make_cloud_path(cr, clouds[c2], line_width,L);
                        cairo_clip(cr);
                    }
                }
            }
	    else //(t == 2)
	    {
		cairo_set_line_width(cr, line_width/2.0);
		cairo_set_dash (cr, dashes, 2, 0.0);
	    }

	    // For crossing diagnostics: if (e_cross_v[e]) cairo_set_source_rgb(cr, 1, 0, 0);
      
	    cairo_move_to (cr, x1, y1);
	    cairo_line_to (cr, x2, y2);

	    cairo_stroke (cr);
	}
	cairo_restore(cr);
    }

    draw_node_circles(cr, L, line_width);

    cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, font_size()/scale);

    vector<tip_label> labels;
    point_position center(L.x_center(), L.y_center());
    for (int l=0; l<L.MC.n_leaves(); ++l)
    {
        int node = L.MC.leaf(l);
        int branch = L.MC.branch_to_node(node);
        int neighbor = L.MC.mapping[L.MC.reverse(branch)];
        labels.push_back(measure_tip_label(cr, L.MC.names()[l], L.node_positions[node],
            L.node_positions[neighbor], L.node_radius[node], center, line_width));
    }
    vector<std::pair<point_position,point_position>> edges;
    for (const auto& edge: L.MC.edges)
        if (edge.type != 2 or draw_type_2_edges)
            edges.emplace_back(L.node_positions[edge.from], L.node_positions[edge.to]);
    draw_horizontal_labels(cr, labels, L, edges, pt);
}


void draw_graph(const MC_tree_with_lengths& T,const string& name)
{
    // try to scale lengths in the *.dot file so that the average is 1.0
    double scale = 1.0;
    double total_branch_length = 0;
    for(int b=0;b<T.n_branches();b++)
	total_branch_length += T.branch_length(b);
    // Zero-length trees still have valid text exports; keep their DOT lengths zero.
    if (total_branch_length != 0)
        scale = 0.5*T.n_branches()/total_branch_length;

    const int N = T.n_nodes();

    cout<<"digraph "<<std::quoted(name)<<" { \n\
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
	    const partition& p = T.partitions[e.partition];
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
	cout<<"      N"<<T.leaf(i)<<" [label="<<std::quoted(names[i])<<"]\n";
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
    vector<partition> full;
    for(int i=0;i<MC1.branch_order.size();i++) {
	int b = MC1.branch_order[i];
	if (MC1.partitions[b].full())
	    full.push_back(MC1.partitions[b]);
    }

    MC_tree_with_lengths MC2({MC1.names(), full});
  

    // what node in MC2 do nodes in MC1 map to?
    vector<int> node_mapping(MC1.n_nodes(),-1);
    vector<int> branch_mapping(MC1.n_branches(),-1);

    // map full branches in MC1 -> MC2
    for(int i1=0;i1<MC1.n_branches();i1++)
    {
	int b1 = MC1.branch_order[i1];

	if (not MC1.partitions[b1].full()) continue;

	auto b2 = find_index(MC2.partitions,MC1.partitions[b1]);
	if (not b2)
	    throw myexception()<<"Can't find full partition in collapsed tree!\n"<<Partition(MC1.names(), MC1.partitions[b1]);

	branch_mapping[i1] = *b2;

	int left1 = MC1.mapping[MC1.reverse(b1)];
	int right1 = MC1.mapping[b1];

	int left2 = MC2.mapping[MC2.reverse(*b2)];
	int right2 = MC2.mapping[*b2];

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

// Report Cairo failures before discarding the context or finishing the output surface.
void check_cairo(cairo_status_t status)
{
    if (status != CAIRO_STATUS_SUCCESS)
        throw myexception()<<"Cairo: "<<cairo_status_to_string(status);
}

// Record once in points, then fit the padded ink bounds to any explicitly requested dimensions.
// With no dimensions, translation alone preserves physical font/stroke sizes. One dimension
// fixes a uniform scale and derives the other; two dimensions use the smaller scale and center
// the result. Thus the complete padded drawing fits without stretching or clipping its content.
void draw(const string& filename, const string& type, cairo_plotter& plot,
          std::optional<double> width, std::optional<double> height)
{
    if (type != "pdf" and type != "ps" and type != "svg")
        throw myexception()<<"Unrecognized output type '"<<type<<"'; use ps, pdf, or svg.";

    auto recording = std::unique_ptr<cairo_surface_t, decltype(&cairo_surface_destroy)>(
        cairo_recording_surface_create(CAIRO_CONTENT_COLOR_ALPHA, nullptr), cairo_surface_destroy);
    auto cr = std::unique_ptr<cairo_t, decltype(&cairo_destroy)>(cairo_create(recording.get()), cairo_destroy);
    plot(cr.get());
    check_cairo(cairo_status(cr.get()));
    check_cairo(cairo_surface_status(recording.get()));

    double x, y, ink_width, ink_height;
    cairo_recording_surface_ink_extents(recording.get(), &x, &y, &ink_width, &ink_height);
    const double margin = 6; // Points of breathing room; scales with explicitly resized output.
    const double natural_width = ink_width + 2*margin;
    const double natural_height = ink_height + 2*margin;
    double scale = 1;
    if (width and height)
        scale = std::min(*width*inch/natural_width, *height*inch/natural_height);
    else if (width)
        scale = *width*inch/natural_width;
    else if (height)
        scale = *height*inch/natural_height;
    const double page_width = width ? *width*inch : scale*natural_width;
    const double page_height = height ? *height*inch : scale*natural_height;

    const string path = filename + "." + type;
    cairo_surface_t* raw_surface = nullptr;
    if (type == "pdf")
        raw_surface = cairo_pdf_surface_create(path.c_str(), page_width, page_height);
    else if (type == "ps")
        raw_surface = cairo_ps_surface_create(path.c_str(), page_width, page_height);
    else
        raw_surface = cairo_svg_surface_create(path.c_str(), page_width, page_height);
    auto surface = std::unique_ptr<cairo_surface_t, decltype(&cairo_surface_destroy)>(
        raw_surface, cairo_surface_destroy);
    check_cairo(cairo_surface_status(surface.get()));
    auto output = std::unique_ptr<cairo_t, decltype(&cairo_destroy)>(cairo_create(surface.get()), cairo_destroy);
    cairo_translate(output.get(), (page_width-scale*natural_width)/2 + scale*(margin-x),
                    (page_height-scale*natural_height)/2 + scale*(margin-y));
    cairo_scale(output.get(), scale, scale);
    cairo_set_source_surface(output.get(), recording.get(), 0, 0);
    cairo_paint(output.get());
    cairo_show_page(output.get());
    check_cairo(cairo_status(output.get()));
    cairo_surface_finish(surface.get());
    check_cairo(cairo_surface_status(surface.get()));
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
#if defined(HAVE_FEEANBLEECEPT) && !defined(NDEBUG)
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
    
	std::optional<double> width, height;
        if (args.count("width")) width = args["width"].as<double>();
        if (args.count("height")) height = args["height"].as<double>();
        if ((width and (not std::isfinite(*width) or *width <= 0)) or
            (height and (not std::isfinite(*height) or *height <= 0)))
            throw myexception()<<"Output dimensions must be finite and positive.";

        // Letter dimensions remain an initial geometry reference, not page boundaries.
        // A single requested dimension affects only final scaling, preserving the natural aspect.
        double xw = width and height ? *width : 8.5;
        double yw = width and height ? *height : 11;
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
	    assert(not std::isnan(MC.node_length(i)));


	if (args.count("collapse"))
	    MC = collapse_nodes(MC);

	if (output == "dot")
	{
	    draw_graph(MC,get_graph_name(filename));
	    return 0;
	}
	else if (output == "topology")
	{
	    cout<<MC.T.write(false)<<endl;
	    return 0;
	}
	else if (output == "tree")
	{
	    cout<<MC.T.write(true)<<endl;
	    return 0;
	}
	else if (output == "mtree")
	{
	    cout<<MC.T.write(false)<<endl;
	    for(int i=0;i<MC.branch_order.size();i++) {
		const partition& P = MC.partitions[MC.branch_order[i]];
		if (not P.full())
		    cout<<Partition(MC.names(),P)<<endl;
	    }
	    return 0;
	}
	else if (output == "lengths")
	{
	    for(int i=0;i<MC.branch_order.size();i++) {
		int b = MC.branch_order[i];
		const partition& P = MC.partitions[b];
		cout<<"branch "<<MC.branch_length(b)<<"\n";
		cout<<Partition(MC.names(),P)<<endl;
	    }
	    for(int n=0;n<MC.n_nodes();n++) {
		if (MC.node_length(n) > 0) {
		    cout<<"node "<<MC.node_length(n)<<"\n";
		    cout<<Partition(MC.names(),MC.partitions[MC.branch_to_node(n)])<<endl;
		}
	    }
	    return 0;
	}



        const string layout_name = args["layout"].as<string>();
        if (layout_name != "graph" and layout_name != "equal-angle" and layout_name != "equal-daylight")
            throw myexception()<<"Unrecognized layout '"<<layout_name
                               <<"'; use graph, equal-angle, or equal-daylight.";

        bool all_zero = true;
        for(int b: MC.branch_order)
            all_zero = all_zero and MC.branch_length(b) == 0;
        for(int n=0;n<MC.n_nodes();n++)
            all_zero = all_zero and MC.node_length(n) == 0;
        if (all_zero)
            throw myexception()<<"Cannot draw a tree whose branch and node lengths are all zero.";

        if (layout_name != "graph")
        {
            bool greedy = args.count("greedy") > 0;
            bool no_shade = args.count("no-shade") > 0;
            tree_layout L = layout_name == "equal-angle" ? equal_angle_layout(MC)
                                                       : equal_daylight_layout(MC,greedy,no_shade);
            L.rotate_for_aspect_ratio(xw,yw);
            tree_plotter tp(L, xw, yw, font_size);
            tp.horizontal_names = args["labels"].as<string>() == "horizontal";
            string filename = args.count("out") ? args["out"].as<string>() : name+"-tree";
            draw(filename,output,tp,width,height);
            return 0;
        }

	// lay out as a graph
	MC.scale(1.0/MC.total_length());
	graph_layout layout = layout_on_circle(MC,2);
    
	if (args.count("tree-layout-initial"))
	{
	    tree_layout LA = equal_angle_layout(MC);
	    //cout<<"equal angle edge error = "<<LA.edge_length_error()<<endl;
	    // *Transfer tree layout to graph layout* -- this only works when the graph IS a tree
	    // otherwise, multiple graph nodes would end up at the same position
	    for(int i=0;i<MC.C;i++)
		layout.node_positions[i] = LA.node_positions[MC.graph_node_to_tree_node(i)];
	}


	double length_weight = 10000000;

	int n_iterations = args["iterations"].as<int>();
	/*
	  for(int i=0;i<10;i++) {
	  double w = length_weight/100*double(i)/10;
	  layout = energy_layout(layout,layout_energy(1,w,1,0),1000);
	  }
	*/

	double stretchy_weight = 1.0;
	if (args.count("draw-clouds") and args["draw-clouds"].as<string>() == "only") {
	    stretchy_weight = 0.1;

	}


	// FIXME - the repulsion should actually depend on the number of nodes!
	if (not args.count("tree-layout-initial")) {
	    energy_layout(layout,layout_energy(1,1000,stretchy_weight,0),400);
	    energy_layout(layout,layout_energy(1,10000,stretchy_weight,0),400);
	    energy_layout(layout,layout_energy(1,100000,stretchy_weight,0),400);
	    energy_layout(layout,layout_energy(1,1000000,stretchy_weight,0),800);
	}

        for(int i=0;i<n_iterations;i++)
        {
            energy_layout(layout,layout_energy(1,length_weight,stretchy_weight,0));
            layout.rotate_for_aspect_ratio(xw,yw);
        }

        // Improve angular resolution without changing the preceding optimization sequence.
        int a_iterations = args["angle_iterations"].as<int>();
        for(int i=0;i<a_iterations;i++)
        {
            energy_layout(layout,layout_energy(1,10000000,0.01,50));
            layout.rotate_for_aspect_ratio(xw,yw);
        }

        // Render the final layout once, including when neither loop has any iterations.
        graph_plotter gp(layout, xw, yw, font_size);
        if (args.count("draw-clouds"))
        {
            gp.draw_clouds = true;
            gp.draw_type_2_edges = args["draw-clouds"].as<string>() != "only";
        }
        string output_name = args.count("out") ? args["out"].as<string>() : name+"-mctree";
        draw(output_name,output,gp,width,height);
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
