#include <iostream>
#include "util/cmdline.H"
#include "util/myexception.H"
#include "util/range.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/string/pred.H"
#include "util/string/convert.H"

using std::vector;
using std::string;

void check_supplied_filenames(int n,const vector<string>& files, bool exact)
{
    if (files.size() == n)
	return;

    if (files.size() < n)
	throw myexception()<<"Wanted "<<n<<" filenames, but got only "<<files.size()<<".";
    if (exact and files.size() > n)
	std::cerr<<"Warning: ignoring "<<files.size()-n<<" extra filenames."<<std::endl;
}


/// \brief Parse a range of the form <begin>-<end> which should be a subset of [1,L]
///
/// \param range The string to parse.
/// \param L The upper bound.
/// \param begin Parameter for passing back the beginning of the range.
/// \param end Parameter for passing back the end of the range.
///
void parse_simple_range(const string& range,int L,int& begin,int& end)
{
    vector<string> R = split(range,'-');

    if (R.size() == 1) {
	begin = end = convertTo<int>(range)-1;
    }
    else if (R.size() != 2)
	throw myexception()<<"Malformed range '"<<range<<"'";
    else {
	begin = 0;
	if (R[0].size())
	    begin = convertTo<int>(R[0])-1;

	end = L-1;
	if (R[1].size())
	    end = convertTo<int>(R[1])-1;
    }
    
    if (begin < 0)
	throw myexception()<<"Bad range '"<<range<<"': begins before 1.";
    
    if (begin > L-1)
	throw myexception()<<"Bad range '"<<range<<"': begins after end of sequence (L="<<L<<").";
    
    if (end < 0)
	throw myexception()<<"Bad range '"<<range<<"': ends before 1!";
    
    if (end > L-1)
	throw myexception()<<"Bad range '"<<range<<"': ends after end of sequence (L="<<L<<").";
    
    if (end < begin)
	throw myexception()<<"Bad range '"<<range<<"': begins after end!";
}

/// \brief Parse a range of the form <begin>-<end>/<step> which should be a subset of [1,L]
///
/// \param range The string to parse.
/// \param L The upper bound.
/// \param begin Parameter for passing back the beginning of the range.
/// \param end Parameter for passing back the end of the range.
/// \param end Parameter for passing back the step size.
///
void parse_modulo_range(const string& range,int L,int& begin, int& end, int& step)
{
    vector<string> R = split(range,'/');

    if (R.size() == 1) 
	step = 1;
    else if (R.size() == 2) {
	try {
	    step = convertTo<int>(R[1]);
	}
	catch (...) {
	    throw myexception()<<"Malformed step size '"<<R[1]<<"' in range '"<<range<<"'";
	}
	if (step < 1)
	    throw myexception()<<"Step is not positive in range '"<<range<<"'";
    }
    else
	throw myexception()<<"Malformed range '"<<range<<"'";

    parse_simple_range(R[0],L,begin,end);
}

/// \brief Parse a comma-separated list of ranges <begin>-<end>/<step> and construct an ordered list.
///
/// \param range The string to parse.
/// \param L The upper bound.
/// \return On ordered list constructed by concatenating the elements in the individual ranges.
///
vector<int> parse_multi_range(const string& range,int L)
{
    vector<string> ranges = split(range,',');

    vector<int> columns;
    for(int i=0;i<ranges.size();i++) 
    {
	int begin = -1;
	int end = -1;
	int step = -1;

	parse_modulo_range(ranges[i], L, begin, end, step);
    
	for(int c=begin;c<=end;c++)
	    if ((c-begin)%step == 0)
		columns.push_back(c);
    }
    return columns;
}

vector<string> get_arguments(string& s,char begin, char end)
{
    if (not s.size() or s[s.size()-1] != end) 
	return vector<string>();
  
    int loc = s.find(begin);
    string args = s.substr(loc+1, s.size()-loc-2);

    if (loc == -1) 
	return vector<string>();
    else
	s = s.substr(0,loc);
  
    return split(args,',');
}

vector<string> parse_string_list(const std::string& values_str)
{
    if (values_str.empty()) return {};

    auto values = resplit(values_str, R"([\n,])");
    values = select(values, [](auto& x){return not x.empty();});
    return values;
}

std::vector<std::string> get_string_list(const boost::program_options::variables_map& args, const std::string& key)
{
    if (auto str = get_arg<string>(args,key))
    {
	// Discard the '@'
	if (starts_with(*str,"@"))
	    str = read_file(str->substr(1));
	// Keep the '@'
	else if (starts_with(*str,R"(\@)"))
	    str = str->substr(1);
	return parse_string_list(*str);
    }
    else
	return {};
}

