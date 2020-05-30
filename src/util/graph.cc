#include "util/graph.H"

using std::vector;

Graph get_subgraph(const vector<int> vertices, const Graph& graph)
{
    Graph subgraph(vertices.size());
    for(int i=0;i<vertices.size();i++)
	for(int j=0;j<vertices.size();j++)
	    if (edge(vertices[i],vertices[j],graph).second)
		boost::add_edge(i, j, subgraph);
    return subgraph;
}

vector<vector<int>> get_ordered_strong_components(const Graph& graph)
{
    using namespace boost;
    const int L = num_vertices(graph);

    // 1. Label each vertex with its component
    vector<int> component_for_index(L);
    int C = strong_components(graph, make_iterator_property_map(component_for_index.begin(), get(vertex_index, graph)));

    // find live variables in each component
    vector<vector<int>> components(C);

    for(int i=0;i<L;i++)
    {
	int c = component_for_index[i];
	components[c].push_back(i);
    }

    return components;
}

vector<int> topo_sort(const Graph& graph)
{
    using namespace boost;

    vector<Vertex> sorted_vertices;
    topological_sort(graph, std::back_inserter(sorted_vertices));

    vector<int> sorted_indices(sorted_vertices.size());
    for(int i=0;i<sorted_indices.size();i++)
	sorted_indices[i] = get(vertex_index, graph, sorted_vertices[i]);

    return sorted_indices;
}

