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
#include "pow2.H"
#include "mctree.H"
#include "util.H"

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

const double inch = 72.0;

struct cairo_plotter
{
  virtual void operator()(cairo_t*) = 0;
  virtual ~cairo_plotter() {}
};

struct fn_plotter: public cairo_plotter
{
  void (*f)(cairo_t*);
  void operator()(cairo_t* cr) {f(cr);}
  fn_plotter(void (*f1)(cairo_t*)):f(f1) {}

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
  cairo_surface_t *surface = cairo_ps_surface_create(filename.c_str(),8.5*inch, 11*inch);

  draw_to_page(surface,draw);
}

void draw_to_pdf(const string& filename, cairo_plotter& draw)
{
  cairo_surface_t *surface = cairo_pdf_surface_create(filename.c_str(),8.5*inch, 11*inch);

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
    ("output",value<string>(),"DOT,PS,PDF")
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

  //  vector<Partition> mini_branches;
  //  vector<double> mini_branch_lengths;
  //  vector<int> parent_branch_of_mini_branch;
  MC_tree_with_lengths(const vector<Partition>& branches)
    :MC_tree(branches),
     branch_lengths(2*n_branches(),0.0),
     node_lengths(n_nodes(),0.0)
  { }
};

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
      branches.push_back(P);
      branch_lengths.push_back(length);
    }
    else if (words[0] == "node") {
      nodes.push_back(P);
      node_lengths.push_back(length);
    }
    else if (words[0] == "mini-branch") {
      mini_branches.push_back(P);
      mini_branch_lengths.push_back(length);
    }
    else
      throw myexception()<<"Don't understand word="<<words[0];
  }

  cerr<<"Read "<<branches.size()<<" partitions"<<endl;

  //--------------- Construct MC tree with lengths  ---------------//
  MC_tree_with_lengths MC(check_MC_partitions(branches));

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

    int b = find_index(MC.partitions,nodes[i]);
    if (b == -1)
      throw myexception()<<"Can't find node in tree!\n"<<"    "<<nodes[i];

    int n = MC.mapping[b];
    cerr<<"Processing node '"<<nodes[i]<<"'"<<endl;
    cerr<<"degree = "<<MC.degree(n)<<endl;
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
};


// FIXME - how to handle text layout?
//        (a) problem - we don't know the text sizes in the tree layout!
//        (b) we could try to find the page boundaries in the page drawing code.
//        (c) but then layout algorithms couldn't try to prevent label overlap.

// 1. FIXME - figure out how much angle space we REALLY have!
// 2. FIXME - give away unused angle space...
// 3. Place short branches in a cluster near the edge of our angle space,
//    place the longest ones in the middle!

// 4. HOW hard would it be to allow manual adjustment of the placements???

struct tree_layout 
{
  SequenceTree T;
  vector<double> node_radius;
  vector<point_position> node_positions;
  tree_layout(const SequenceTree& T1)
    :T(T1),
     node_radius(T.n_nodes(),0),
     node_positions(T.n_nodes())
  {}
};


int find_directed_branch(const Tree& T,const Partition& p)
{
  for(int b=0; b<2*T.n_branches(); b++)
    if (equal(branch_partition(T,b), p.group2))
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
  return n_elements(branch_partition(T,b));
}


void circular_layout(tree_layout& L,int parent,double min_a,double max_a)
{
  SequenceTree& T = L.T;

  if (n_children(T,parent) == 1) return;

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

    circular_layout(L,*b,a1,a2);
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
double node_diameter(double lengths,int degree)
{
  double ratio = 1.0;

  if (degree == 5)
    ratio = 0.75;
  else if (degree > 5)
    ratio = 2.0*(1.0 + log2(degree) - log2(3))/(2.0*degree - 3.0);

  return lengths*ratio;
}

tree_layout circular_layout(SequenceTree MF,const vector<double>& node_radius)
{
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
  
  cerr<<"root = "<<root<<endl;

  tree_layout L(MF);
  L.node_radius = node_radius;

  double a1 = 0;
  for(out_edges_iterator b = MF[root].branches_out();b;b++) 
  {
    double a2 = a1 + 2.0 * M_PI * n_children(MF,*b)/MF.n_leaves();
    double a = (a1+a2)/2.0;
    
    cerr<<"branch "<<(*b).name()<<"  n_children = "<<n_children(MF,*b)<<endl;

    int n1 = (*b).source();
    int n2 = (*b).target();
    double length = (*b).length();

    L.node_positions[n2] = L.node_positions[n1];
    L.node_positions[n2].x += length*cos(a);
    L.node_positions[n2].y += length*sin(a);

    cerr<<"edge target = "<<n2<<"   a1 = "<<a1*180/M_PI<<" a = "<<a*360/(2*M_PI)<<" a2 = "<<a2*180/M_PI<<"   length = "<<length<<"  x = "<<    L.node_positions[n2].x<<" y = "<<L.node_positions[n2].x<<endl;
    circular_layout(L,*b,a1,a2);
    a1 = a2;
  }
  return L;
}

tree_layout circular_layout(const MC_tree_with_lengths& MC)
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
    int d = MF[mf_n].degree();
    node_radius[mf_n] = node_diameter(MC.node_length(n), d)/2.0;
  }

  return circular_layout(MF,node_radius);
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
  tree_layout L;
  void operator()(cairo_t*);
  tree_plotter(const tree_layout& tl):L(tl) {}
};

void tree_plotter::operator()(cairo_t* cr)
{
  double xmin = L.node_positions[0].x;
  double xmax = L.node_positions[0].x;
  for(int i=0;i<L.node_positions.size();i++)
  {
    xmin = min(xmin,L.node_positions[i].x);
    xmax = max(xmax,L.node_positions[i].x);
  }
    
  double ymin = L.node_positions[0].y;
  double ymax = L.node_positions[0].y;
  for(int i=0;i<L.node_positions.size();i++)
  {
    ymin = min(ymin,L.node_positions[i].y);
    ymax = max(ymax,L.node_positions[i].y);
  }
  cout<<"x = ["<<xmin<<", "<<xmax<<"]"<<endl;
  cout<<"y = ["<<ymin<<", "<<ymax<<"]"<<endl;
  cout<<endl;

  // line width 5 points
  const double line_width = 0.005;
  cairo_set_line_width(cr, line_width);
  cairo_set_line_cap  (cr, CAIRO_LINE_CAP_ROUND);

  // move to center and flip up 
  cairo_translate(cr, 8.5*inch/2.0, 11.0*inch/2.0);
  //  cairo_scale(cr, 1, -1);
  //  cairo_set_line_width(cr, 0.04);

  // find scaling factor
  const double edge = 0.05;
  const double factor = 1.0-edge*2;

  double scale = std::min(8.5*inch/(xmax-xmin),11.5*inch/(ymax-ymin));
  scale *= factor;
  cairo_scale(cr, scale, scale);
  cairo_translate(cr, factor*-0.5*(xmax+xmin), factor*-0.5*(ymax+ymin));
  

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

  const double dashes[] = {line_width*3, line_width*3};

  cairo_set_dash (cr, dashes, 2, 0.0);

  // draw the circles
  for(int n=0;n<L.node_radius.size();n++) 
  {
    double x = L.node_positions[n].x;
    double y = L.node_positions[n].y;

    if (L.node_radius[n] > 0) {
      cerr<<"node n="<<n<<"  radius = "<<L.node_radius[n]<<endl;
      cairo_arc(cr, x, y, L.node_radius[n], 0.0, 2.0 * M_PI);

      cairo_set_source_rgb (cr, 1 , 1, 1);
      cairo_fill_preserve (cr);

      cairo_set_source_rgb (cr, 0 , 0, 0);
      cairo_stroke(cr);
    }
  }

    cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size (cr, 0.025);

  for(int l=0;l<L.T.n_leaves();l++)
  {  
    double x1 = L.node_positions[l].x;
    double y1 = L.node_positions[l].y;

    int p = L.T.branch(l).target();

    double x2 = L.node_positions[p].x;
    double y2 = L.node_positions[p].y;

    double a = atan2(y1-y2,x1-x2);

    double W = get_text_length(cr, L.T.seq(l));
    double H = get_text_height(cr, L.T.seq(l));

    x1 += cos(a)*line_width;
    y1 += sin(a)*line_width;

    y1 += 0.5*H;

    if (abs(cos(a) < 0.4))
    {
      if (cos(a) < 0)
	x1 -= W;
      
      if (sin(a) < 0)
	y1 -= H;
      else
	y1 += H;
    }

    cairo_move_to (cr, x1, y1);
    cairo_show_text (cr, L.T.seq(l).c_str());
  }

    
 
}

struct graph_layout
{
  MC_tree_with_lengths MC;
  vector<double> node_radius;
  vector<point_position> node_positions;
  graph_layout(const MC_tree_with_lengths& T)
    :MC(T),
     node_radius(MC.n_nodes(),0),
     node_positions(MC.n_nodes())
  {
    //determine total length
    double t = 0;
    for(int i=0;i<MC.branch_order.size();i++) 
    {
      int b = MC.branch_order[i];
      t += MC.branch_length(b);
    }
    for(int n=0;n<MC.n_nodes();n++) 
      t += MC.node_length(n);

    // rescale length to 1
    for(int b=0;b<2*MC.n_branches();b++) 
      MC.branch_length(b) /= t;
    for(int n=0;n<MC.n_nodes();n++) 
      MC.node_length(n) /= t;
    
    // determine node radii
    for(int n=0;n<MC.n_nodes();n++)
      node_radius[n] = node_diameter(MC.node_length(n),MC.degree(n))/2.0;
  }
};


graph_layout layout_on_circle(MC_tree_with_lengths& MC)
{
  graph_layout GL(MC);

  for(int i=0;i<MC.n_nodes();i++)
  {
    double angle = i*(2.0*M_PI/MC.n_nodes());
    GL.node_positions[i].x = cos(angle);
    GL.node_positions[i].y = sin(angle);
  }
  return GL;
}

struct graph_energy_function
{
  virtual double operator()(const graph_layout& GL) const=0;
  virtual ~graph_energy_function() {}
};


double distance(const graph_layout& GL,int n1,int n2)
{
  double x1 = GL.node_positions[n1].x;
  double y1 = GL.node_positions[n1].y;

  double x2 = GL.node_positions[n2].x;
  double y2 = GL.node_positions[n2].y;

  double L = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));

  return L;
}

struct energy1: public graph_energy_function
{
  double operator()(const graph_layout& GL) const 
  {
    /// edge length energies
    double edge_energy =0;
    for(int e=0;e<GL.MC.edges.size();e++) 
    {
      int n1 = GL.MC.edges[e].from;
      int n2 = GL.MC.edges[e].to;
      int b = GL.MC.edges[e].partition;

      double L = (distance(GL,n1,n2) - GL.node_radius[n1] - GL.node_radius[n2]);

      if (L < 0) edge_energy += L*L;

      double L2 = 0;
      if (b >= 0)
	L2 = GL.MC.branch_length(b);

      //      cerr<<"edge "<<e<<": "<<n1<<" <-> "<<n2<<" b = "<<b<<"    L = "<<L<<"  L2 = "<<L2<<endl;

      edge_energy += (L-L2)*(L-L2);
      if (L < 0) edge_energy += L*L;
    }

    /// node_distances
    double node_energy = 0;
    for(int i=0;i<GL.MC.n_nodes();i++)
      for(int j=0;j<i;j++)
      {
	// don't repel the end of the branch that I'm attached to
	if (GL.MC.connected(i,j) == 1) continue;

	// don't repel nodes that wander over me, or that I wander to
	if (GL.MC.connected(i,j) == 2 or GL.MC.connected(j,i) == 2) continue;

	double L = distance(GL,i,j);

	node_energy += 1.0/std::abs(L);
      }

    return node_energy + edge_energy*1000000.0;
  }
};


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

double distance_to_line_segment(const graph_layout& GL,int n1,int n2,int n3)
{
  return distance_to_line_segment(GL.node_positions[n1],
				  GL.node_positions[n2],
				  GL.node_positions[n3]);
}



struct energy2: public graph_energy_function
{
  double length_weight;
  double down_weight_stretchy;

  double operator()(const graph_layout& GL) const 
  {
    /// edge length energies (type 1)
    double edge_energy =0;

    double stretchy_edge_energy = 0;

    const double closest_fraction = 0.9;

    for(int e=0;e<GL.MC.edges.size();e++) 
    {
      int n1 = GL.MC.edges[e].from;
      int n2 = GL.MC.edges[e].to;
      int b = GL.MC.edges[e].partition;
      int t = GL.MC.edges[e].type;

      double L = distance(GL,n1,n2) - GL.node_radius[n1] - GL.node_radius[n2];

      if (L < 0) edge_energy += L*L;

      double L2 = 0;
      double weight = 1.0;
      if (t == 1)
	L2 = GL.MC.branch_length(b);
      else {
	int b = GL.MC.branch_to_node(GL.MC.edges[e].from);
	weight = 1.0/(GL.MC.directly_wanders[b] + 1);
	weight *= (1-closest_fraction);
      }

      //      cerr<<"edge "<<e<<": "<<n1<<" <-> "<<n2<<" b = "<<b<<"    L = "<<L<<"  L2 = "<<L2<<endl;

      double E = (L-L2)*(L-L2)*weight;

      if (t == 1)
	edge_energy += E;
      else
	stretchy_edge_energy += E;
    }

    /// edge length energies (type 2)
    for(int b = 0;b<GL.MC.n_branches()*2;b++) 
    {
      if (not GL.MC.directly_wanders[b]) continue;
	
      int n1 = GL.MC.mapping[b];

      vector<double> distances;
      for(int i = 0;i<GL.MC.branch_order.size();i++) 
      {
	int b2 = GL.MC.branch_order[i];

	int n2 = GL.MC.mapping[b2];
	int n3 = GL.MC.mapping[GL.MC.reverse(b2)];
	
	double d = distance_to_line_segment(GL,n1,n2,n3);

	distances.push_back(d);
      }

      double D = min(distances);
      stretchy_edge_energy += D*D*closest_fraction;
    }

    /// node_distances
    double node_energy = 0;
    for(int i=0;i<GL.MC.n_nodes();i++)
      for(int j=0;j<i;j++)
      {
	// don't repel the end of the branch that I'm attached to
	if (GL.MC.connected(i,j) == 1) continue;

	// don't repel nodes that wander over me, or that I wander to
	if (GL.MC.connected(i,j) == 2 or GL.MC.connected(j,i) == 2) continue;

	double L = distance(GL,i,j);

	node_energy += 1.0/std::abs(L);
      }

    return node_energy + (edge_energy + stretchy_edge_energy/down_weight_stretchy)*length_weight;
  }

  energy2(double W1,double W2):length_weight(W1),down_weight_stretchy(W2) {}
};


vector<point_position> energy_derivative_2D(const graph_layout& GL1, const graph_energy_function& E)
{
  double DELTA = 0.0001;

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

vector<point_position> add(const vector<point_position>& x1,double lambda,const vector<point_position>& v)
{
  vector<point_position> x2 = x1;
  for(int i=0;i<x2.size();i++) {
    x2[i].x += lambda * v[i].x;
    x2[i].y += lambda * v[i].y;
    x2[i].z += lambda * v[i].z;
  }

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

graph_layout energy_layout(graph_layout GL, const graph_energy_function& E)
{
  double T = 0;
  double dt = 1.0;
  int successes=0;

  for(int i=0;i<1000;i++) {
    double E1 = E(GL);
    cerr<<"iter = "<<i<<" E = "<<E1<<"   T = "<<T<<endl;
    vector<point_position> D = energy_derivative_2D(GL,E);
    vector<point_position> temp = GL.node_positions;
    double delta = dt*sqrt(E1)/dot(D,D);
    GL.node_positions = add(GL.node_positions,-delta*sqrt(E1),D);
    double E2 = E(GL);
    if (E2 > E1) {
      cerr<<"    rejecting  E = "<<E2<<endl;
      GL.node_positions = temp;
      dt /= 2.0;
      successes=0;
    } 
    else {
      T += delta;
      successes++;
      if (successes>3) {
	dt *= 2.0;
	successes=0;
      }
    }
    if (sqrt(dot(D,D)) < 1.0e-3)
      break;
  }

  return GL;
}

struct graph_plotter: public cairo_plotter
{
  graph_layout L;
  void operator()(cairo_t*);
  graph_plotter(const graph_layout& gl):L(gl) {}
};

void graph_plotter::operator()(cairo_t* cr)
{
  double xmin = L.node_positions[0].x;
  double xmax = L.node_positions[0].x;
  for(int i=0;i<L.node_positions.size();i++)
  {
    xmin = min(xmin,L.node_positions[i].x);
    xmax = max(xmax,L.node_positions[i].x);
  }
    
  double ymin = L.node_positions[0].y;
  double ymax = L.node_positions[0].y;
  for(int i=0;i<L.node_positions.size();i++)
  {
    ymin = min(ymin,L.node_positions[i].y);
    ymax = max(ymax,L.node_positions[i].y);
  }
  //  cout<<"x = ["<<xmin<<", "<<xmax<<"]"<<endl;
  //  cout<<"y = ["<<ymin<<", "<<ymax<<"]"<<endl;
  //  cout<<endl;

  // line width 5 points
  //  cairo_set_line_cap  (cr, CAIRO_LINE_CAP_ROUND);

  // move to center and flip up 
  cairo_translate(cr, 8.5*inch/2.0, 11.0*inch/2.0);
  //  cairo_scale(cr, 1, -1);
  //cairo_set_line_width(cr, 0.04);

  // find scaling factor
  const double edge = 0.10;
  const double factor = 1.0-edge*2;

  double scale = std::min(8.5*inch/(xmax-xmin),11.5*inch/(ymax-ymin));
  scale *= factor;
  const double line_width = 2.0/scale;
  cairo_scale(cr, scale, scale);
  cairo_translate(cr, factor*-0.5*(xmax+xmin), factor*-0.5*(ymax+ymin));
  cairo_set_line_width(cr, line_width);
  
  const double dashes[] = {3.0*line_width, 3.0*line_width};

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

    cairo_save(cr); 
    {
      cairo_move_to (cr, x1, y1);
      cairo_line_to (cr, x2, y2);
      if (t == 1) {
	if (L.MC.partitions[b].full())
	  cairo_set_source_rgb (cr, 0, 0 ,0);
	else
	  cairo_set_source_rgb (cr, 0.3, 1.0 ,0.3);
      }
      else {
	cairo_set_line_width(cr, line_width/2.0);
	cairo_set_dash (cr, dashes, 2, 0.0);
      }
      
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
      cerr<<"node n="<<n<<"  radius = "<<L.node_radius[n]<<endl;
      cairo_arc(cr, x, y, L.node_radius[n], 0.0, 2.0 * M_PI);

      cairo_set_source_rgb (cr, 1 , 1, 1);
      cairo_fill_preserve (cr);

      cairo_set_source_rgb (cr, 0 , 0, 0);
      cairo_set_line_width(cr, line_width/2.0);
      cairo_stroke(cr);
    }
  }

  cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
  cairo_set_font_size (cr, 10.0/scale);

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

    x1 += cos(a)*line_width;
    y1 += sin(a)*line_width;

    y1 += 0.5*H;

    if (abs(cos(a) < 0.4))
    {
      if (cos(a) < 0)
	x1 -= W;
      
      if (sin(a) < 0)
	y1 -= H;
      else
	y1 += H;
    }

    cairo_move_to (cr, x1, y1);
    cairo_show_text (cr, name.c_str());
  }

    
 
}

// FIXME - page="8.5,11" ?
void draw_graph(const MC_tree_with_lengths& T,const string& name)
{
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
    const edge& e = T.edges[i];
    cout<<"      N"<<e.from<<" -> N"<<e.to;
    int b = e.partition;

    vector<string> attributes;
    vector<string> styles;

    if (e.type == 1) {
      attributes.push_back("arrowhead=none");
      attributes.push_back(string("len=")+convertToString(5.0*T.branch_length(b)));
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


int main(int argc,char* argv[]) 
{
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //-------- Load MC Tree and lengths -----//
    string filename = args["file"].as<string>();

    // FIX name collision!
    MC_tree_with_lengths MC = get_MC_tree_with_lengths(filename);

    if (args.count("output") and args["output"].as<string>() == "DOT")
    {
      draw_graph(MC,get_graph_name(filename));
      exit(1);
    }
      
    /// FIXME! - find some way to plot a MF tree from MC lengths
    /// FIXME! - draw a MF/MC tree w/ no length information

    // lay out the tree
    //    tree_layout L1 = circular_layout(MC);
    //    tree_layout L2 = graph_layout(MC);

    // draw the tree
    //    tree_plotter tp(L1);
    //    draw_to_ps(filename+"-tree.ps",tp);
    //    draw_to_pdf(filename+"-tree.pdf",tp);

    // lay out the tree
    graph_layout L2 = layout_on_circle(MC);

    graph_layout L3 = L2;
    for(int i=0;i<1;i++) {
      L3 = energy_layout(L3,energy2(10000000,100));
      graph_plotter gp(L3);
      draw_to_ps(filename+"-graph.ps",gp);
      draw_to_pdf(filename+"-graph.pdf",gp);
    }
    for(int i=0;i<1;i++) {
      L3 = energy_layout(L3,energy2(20000000,10));
      graph_plotter gp(L3);
      draw_to_ps(filename+"-graph.ps",gp);
      draw_to_pdf(filename+"-graph.pdf",gp);
    }
    for(int i=0;i<10;i++) {
      L3 = energy_layout(L3,energy2(40000000,2.5));
      graph_plotter gp(L3);
      draw_to_ps(filename+"-graph.ps",gp);
      draw_to_pdf(filename+"-graph.pdf",gp);
    }
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
