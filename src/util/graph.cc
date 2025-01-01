#include "util/graph.H"

using std::vector;
using boost::vertex_index;

Graph make_graph(int N, std::function<bool(int,int)> edge_from_to)
{
    Graph G(N);
    for(int i=0;i<N;i++)
        for(int j=0;j<N;j++)
            if (edge_from_to(i,j))
                boost::add_edge(i,j,G);
    return G;
}

Graph get_subgraph(const vector<int>& vertices, const Graph& graph)
{
    auto edge_from_to = [&](int i, int j) { return edge(vertices[i], vertices[j], graph).second; };

    return make_graph(vertices.size(), edge_from_to);
}

vector<vector<int>> get_ordered_strong_components(const Graph& graph)
{
    const int L = num_vertices(graph);

    // 1. Label each vertex with its component
    vector<int> component_for_index(L);
    int C = strong_components(graph, boost::make_iterator_property_map(component_for_index.begin(), get(vertex_index, graph)));

    // find live variables in each component
    vector<vector<int>> components(C);

    for(int i=0;i<L;i++)
    {
	int c = component_for_index[i];
	components[c].push_back(i);
    }

    return components;
}

vector<vector<int>> get_loop_components(const Graph& graph)
{
    vector<vector<int>> loop_components;

    for(auto& component: get_ordered_strong_components(graph))
    {
        if (component.size() == 1 and not edge(component[0], component[0], graph).second) continue;

        loop_components.push_back(std::move(component));
    }

    return loop_components;
}

vector<int> topo_sort(const Graph& graph)
{
    vector<Vertex> sorted_vertices;
    topological_sort(graph, std::back_inserter(sorted_vertices));

    vector<int> sorted_indices(sorted_vertices.size());
    for(int i=0;i<sorted_indices.size();i++)
	sorted_indices[i] = get(vertex_index, graph, sorted_vertices[i]);

    return sorted_indices;
}

