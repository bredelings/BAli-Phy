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
  point_position():x(0),y(0) {}
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
    for(int n=0;n<MC.n_nodes();n++)
      node_radius[n] = node_diameter(MC.node_length(n),MC.degree(n))/2.0;
  }
};


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
  
  const double dashes[] = {line_width*3, line_width*3};

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
	  cairo_set_source_rgb (cr, 0, 1.0 ,0);
      }
      else 
	cairo_set_dash (cr, dashes, 2, 0.0);
      
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
      cairo_stroke(cr);
    }
  }

  cairo_select_font_face (cr, "Sans", 
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_BOLD);
  cairo_set_font_size (cr, 0.025);

  for(int l=0;l<L.MC.n_leaves();l++)
  {  
    string name = L.MC.names()[l];
    int b = L.MC.branch_to_node(l);
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
      node[shape=plaintext,width=auto]\n\n";

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
      attributes.push_back("weight=0");
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
      double R = node_diameter(T.node_length(n),d)/2.0;
      R *= 10.0;
      if (R == 0.0)
	cout<<"      N"<<n<<" [label=\"\",shape=circle,height=0.02,width=0.02,fontsize=1]\n";
      else 
	cout<<"      N"<<n<<" [label=\"\",shape=circle,style=dashed,height="<<R<<",width="<<R<<",fontsize=1]\n";
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
      

    // lay out the tree
    tree_layout L1 = circular_layout(MC);
    //    tree_layout L2 = graph_layout(MC);

    // draw the tree
    tree_plotter tp(L1);
    draw_to_ps(filename+"-tree.ps",tp);
    draw_to_pdf(filename+"-tree.pdf",tp);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
