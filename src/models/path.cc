#include "models/path.H"

#include "util/string/convert.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/ptree.H"

#include <optional>
#include <set>

using std::string;
using std::vector;
using std::optional;
using std::multiset;

const string model_separator = "/";

string model_extend_path(const string& path,const string& x)
{
    if (path.empty())
	return x;
    else
	return path + model_separator + x;
}

string model_path(const vector<string>& path)
{
    return join(path, model_separator);
}

vector<string> model_split_path(const string& path)
{
    return split(path, model_separator);
}

string translate_structures(const string& name)
{
    vector<string> path = model_split_path(name);

    vector<string> path2;
    optional<int> elem;
    for(auto& x: path)
    {
        elem = {};
        if (x == "Pair::first")
        {
            if (path2.empty()) path2.push_back("");
            path2.back() += "[1]";
        }
        else if (x == "Pair::second")
        {
            if (path2.empty()) path2.push_back("");
            path2.back() += "[2]";
        }
        else
            path2.push_back(x);
    }
    return model_path(path2);
}


// We can think of this collection of name lists as a tree.
// - Each name list is a path from the root to a tip.
// - Each node (except the root) has a string name associated with it.
// We consider all child nodes of internal node
//  If the set of grandchild lists under child node C does not overlap with the
//   grandchild lists under any other child node, then we can remove node C.
// We should always prefer to remove deeper nodes first.
//  Thus, leaf nodes should never be removed.
// We therefore consider all internal nodes of the tree, starting
//  with the ones furthest from the root, and remove their children
//  if it is allowable.

void copy_to_vec(const ptree& p, vector<string>& names2, const string& path = "")
{
    if (not p.value_is_empty())
    {
	int i=p.get_value<int>();
	names2[i] = path;
    }

    if (not p.empty())
	for(auto& x: p)
	    copy_to_vec(x.second, names2, model_extend_path(path, x.first));
}

// here we aren't handling '*' prefixes...
void simplify(ptree& p)
{
    if (p.empty()) return;

    // 1. First we simplify all the levels below this level.
    for(auto& x: p)
	simplify(x.second);

    // 2. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name a sibling.
    // We therefore count which names at these levels occur twice and avoid them.
    // NOTE: If we have a situation like {I1/S1, S2/I1} then this approach won't simplify to {S1,I1}.
    multiset<string> names;
    for(auto& x: p)
    {
	names.insert(x.first);
	for(auto& y: x.second)
	    names.insert(y.first);
    }

    // 3. If none of the names in an entry occur twice, then we can move all the
    //    names in that entry up to the top level.
    vector<bool> move_children(p.size(), false);
    for(int i=0; i<p.size(); i++)
    {
	auto& x = p[i];
	if (x.second.empty()) continue;

	bool ok = true;
	for(auto& y: x.second)
	{
	    if (names.count(y.first) > 1)
	    {
		ok = false;
		break;
	    }
	}
	if (ok)
	    move_children[i] = true;
    }

    // 4. Move the children
    ptree p2;
    for(int i=0; i<p.size(); i++)
    {
	auto& x = p[i];

	// 4a. Move the entry w/o changing it.
	if (not move_children[i])
	    p2.push_back(std::move(x));

	// 4b. Move the children
	else
	{
	    for(auto& y: x.second)
		p2.push_back(std::move(y));

	    // 4c. Maybe move the bare entry as well.
	    if (not x.second.value_is_empty())
	    {
		x.second.clear();
		p2.push_back(std::move(x));
	    }
	}
    }

    std::swap(p,p2);
}

void add_path(ptree& p, const vector<string>& path, int value, int first=0)
{
    if (first >= path.size())
	p.put_value(value);
    else
    {
	const string& x = path[first];
	auto child = p.get_child_optional(x);
	if (not child)
	{
	    p.push_back({x, ptree("yo")});
	    child = p.get_child_optional(x);
	}
	assert(child);
	add_path(*child, path, value, first+1);
    }
}

vector<string> short_parameter_names(const vector<string>& names)
{
    // construct the name paths
    ptree paths;
    vector<bool> hidden(names.size(),false);
    for(int i=0;i<names.size();i++)
    {
	string path = names[i];

	if (path.size() and path[0] == '*')
	{
	    path = path.substr(1);
	    hidden[i] = true;
	}

	add_path(paths, model_split_path(path), i);
    }

    // Remove levels that aren't needed for disambiguation
    simplify(paths);

    // Recreate a name vector
    vector<string> names2(names.size());
    copy_to_vec(paths, names2);

    // Put hidden-ness attributes back in.
    for(int i=0;i<names2.size();i++)
	if (hidden[i])
	    names2[i] = string("*") + names2[i];

//    for(int i=0;i<names2.size();i++)
//	std::cerr<<names[i]<<" -> "<<names2[i]<<std::endl;
    return names2;
}

