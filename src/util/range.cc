#include "util/range.H"

using std::vector;

int replace_element(vector<int>& v, const  int e1, int e2)
{
    int where = -1;
    for(int i=0;i<v.size();i++)
    {
	if (v[i] == e1)
	{
	    v[i] = e2;
	    where = i;
	}
    }
    return where;
}

int remove_element(vector<int>& v, const  int e)
{
    int where = -1;
    for(int i=0;i<v.size();)
	if (v[i] == e)
	{
	    v.erase(v.begin() + i);
	    where = i;
	}
	else
	    i++;
    return where;
}

void remove_elements(vector<int>& v, const vector<int>& e)
{
    for(int i=0;i<e.size();i++)
	remove_element(v,e[i]);
}

