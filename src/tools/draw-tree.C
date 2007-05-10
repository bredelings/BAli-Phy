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

  cerr<<"Read "<<MC.branches.size()<<" partitions / "
      <<count(MC.branches,&Partition::full)<<" full partitions."<<endl;
  return MC;
}

SequenceTree build_tree(const MC_tree& MC)
{
  SequenceTree MF = star_tree(MC.branches[0].names);

  for(int i=0;i<MC.branches.size();i++)
    if (MC.branches[i].full()) {
      int b = MF.induce_partition(MC.branches[i].group1);
      MF.branch(b).set_length(MC.branch_lengths[i]);
    }

  //set the branch lengths!

  cout<<MF.write(true)<<endl;

  return MF;
}

struct point_position
{
  double x;
  double y;
  point_position():x(0),y(0) {}
};


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
    if (equal(branch_partition(T,b), p.group1))
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
  for(out_edges_iterator b = T.branch(parent).branches_after();b;b++) 
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

tree_layout circular_layout(const MC_tree& MC)
{
  SequenceTree MF = build_tree(MC);


  // determine node lengths
  vector<double> node_radius(MF.n_nodes(),0);

  for(int i=0;i<MC.nodes.size();i++)
  {
    const Partition& p = MC.nodes[i];
    int b = find_directed_branch(MF,p);
    if (b == -1) {
      cerr<<"Can't find node '"<<p<<"'"<<endl;
      continue;
    }
    int n = MF.directed_branch(b).target();

    node_radius[n] = MC.node_lengths[i]/2.0;
  }

  for(int b=0;b<MF.n_branches();b++) {
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
  cairo_set_line_width(cr, 5);

  // move to center and flip up 
  cairo_translate(cr, 8.5*inch/2.0, 11.0*inch/2.0);
  cairo_scale(cr, 1, -1);
  cairo_set_line_width(cr, 0.04);

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

  const double dashes[] = {0.1,0.1};

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
    
 
}


int main(int argc,char* argv[]) 
{
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //-------- Load MC Tree and lengths -----//
    string filename = args["file"].as<string>();
    MC_tree MC = get_MC_tree(filename);

    // lay out the tree
    tree_layout L = circular_layout(MC);

    // draw the tree
    tree_plotter tp(L);
    draw_to_ps(filename+"-tree.ps",tp);
    draw_to_pdf(filename+"-tree.pdf",tp);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

