#ifndef _UTIL_GRAPH_H
#define _UTIL_GRAPH_H

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

#include <functional>

typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph; 
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge_t;

Graph make_graph(int N, std::function<bool(int,int)> edge_from_to);

template <typename C>
Graph make_graph(const std::vector< C >& edge_from_to)
{
    Graph graph(edge_from_to.size());
    for(int from=0; from<edge_from_to.size(); from++)
        for(auto& to: edge_from_to[from])
            boost::add_edge(from, to, graph);
    return graph;
}


Graph get_subgraph(const std::vector<int>& vertices, const Graph& graph);

std::vector<std::vector<int>> get_ordered_strong_components(const Graph& graph);

std::vector<std::vector<int>> get_loop_components(const Graph& graph);

std::vector<int> topo_sort(const Graph& graph);

template <typename U, typename V>
std::vector< std::vector<V> > map_groups(const std::vector< std::vector<U>>& uss, const std::map<U,V>& f)
{
    std::vector< std::vector<V> > vss;
    for(auto& us: uss)
    {
        std::vector<V> vs;
        for(auto& u: us)
            vs.push_back(f.at(u));
        vss.push_back( std::move( vs ) );
    }
    return vss;
}

template <typename V>
std::vector< std::vector<V> > map_groups(const std::vector< std::vector<int>>& uss, const std::vector<V>& f)
{
    std::vector< std::vector<V> > vss;
    for(int i=0; i<uss.size(); i++)
    {
        std::vector<V> vs;
        for(auto& u: uss[i])
            vs.push_back(f[u]);
        vss.push_back( std::move( vs ) );
    }
    return vss;
}

// Here, each T is associated with a list of other Ts that it references.
// Edges go from referencer -> referencee.
template <typename T>
std::vector< std::vector<T> > get_ordered_strong_components(const std::map<T,std::set<T>>& reference_edges)
{
    // Construct mapping between T and integers.
    std::map<T,int> index_for_t;
    std::map<int,T> t_for_index;
    for(auto& [t,_]: reference_edges)
    {
        int index = index_for_t.size();
        index_for_t[t] = index;
        t_for_index[index] = t;
    }

    Graph graph(reference_edges.size());
    for(auto& [t, referenced_ts]: reference_edges)
    {
        for(auto& referenced_t: referenced_ts)
        {
            int from = index_for_t.at(t);
            int to = index_for_t.at(referenced_t);
            boost::add_edge(from, to, graph);
        }
    }

    auto ordered_groups = get_ordered_strong_components(graph);

    return map_groups( ordered_groups, t_for_index );
}

#endif
