#ifndef UTIL_MAP_H
#define UTIL_MAP_H

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
bool set_if_undef(std::map<std::string,U,T>& m,const std::string& name, const U& value) {
    if (m.find(name) != m.end() )
	return false;
    else {
	m[name] = value;
	return true;
    }
}

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
const U& loadvalue(const std::map<std::string,U,T>& m,const std::string& name, const U& value) 
{
    typename std::map<std::string,U,T>::const_iterator here = m.find(name);
    if (here != m.end() )
	return here->second;
    else
	return value;
}

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
const U& loadvalue(std::map<std::string,U,T>& m,const std::string& name) 
{
    typename std::map<std::string,U,T>::const_iterator here = m.find(name);
    if (here != m.end() )
	return here->second;
    else
	throw myexception()<<"Parameter '"<<name<<"' not set.";
}



#endif
