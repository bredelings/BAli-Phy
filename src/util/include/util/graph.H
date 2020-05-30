#ifndef _UTIL_GRAPH_H
#define _UTIL_GRAPH_H

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph; 
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge_t;

Graph get_subgraph(const std::vector<int> vertices, const Graph& graph);

std::vector<std::vector<int>> get_ordered_strong_components(const Graph& graph);

std::vector<int> topo_sort(const Graph& graph);


#endif
