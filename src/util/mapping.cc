#include "mapping.H"

using std::vector;

vector<int> invert(const vector<int>& mapping) 
{
    vector<int> imapping(mapping.size(),-1);

    for(int i=0;i<imapping.size();i++)
    {
	int j = mapping[i];
	if (j < 0 or j > mapping.size())
	    throw myexception()<<"invert: mapping["<<i<<"] == "<<j<<"!";
	if (imapping[j] != -1)
	    throw myexception()<<"invert: mapping not invertible! Both "<<i<<" and "<<imapping[j]<<" map to "<<j<<"!";
	imapping[j] = i;
    }

    return imapping;
}

vector<int> compose(const vector<int>& mapping1,const vector<int>& mapping2)
{
    assert(mapping1.size() == mapping2.size());

    vector<int> mapping(mapping1.size());

    for(int i=0;i<mapping.size();i++)
    {
	int j = mapping1[i];
	if (j == -1)
	    mapping[i] = -1;
	else if (j<0 or j>=mapping2.size())
	    throw myexception()<<"compose: mapping1["<<i<<"] == "<<j<<"!";
	else
	    mapping[i] = mapping2[j];
    }

    return mapping;
}

/// \brief Check if \a mapping[i] == i
///
/// \param mapping The mapping.
///
bool is_identity(const vector<int>& mapping)
{
    for(int i=0;i<mapping.size();i++)
	if (mapping[i] != i)
	    return false;
    return true;
}

bool in_order(const vector<int>& order)
{
    for(int i=0;i<order.size();i++)
	if (order[i] != i)
	    return false;
    return true;
}

