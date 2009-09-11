#include "stats-table.H"
#include "util.H"
#include "myexception.H"

using namespace std;

vector<string> parse_header(const string& line)
{
  vector<string> headers = split(line,'\t');

  if (headers.size() == 0)
    throw myexception()<<"No column names provided!";

  for(int i=0;i<headers.size();i++)
    if (headers[i].size() == 0)
      throw myexception()<<"The "<<i<<"th column name is blank!";

  return headers;
}


vector<string> read_header(std::istream& file)
{
  string line;
  while (file) 
  {
    getline(file,line);
    if (line.size())
      break;
  }

  return parse_header(line);
}

void write_header(std::ostream& o, const vector<string>& headers)
{
  for(int i=0;i<headers.size();i++) 
  {
    cout<<headers[i];
      
    if (i == headers.size()-1)
      o<<"\n";
    else
      o<<"\t";
  }
}

void stats_table::add_row(const vector<double>& row)
{
  assert(row.size() == n_columns());

  for(int i=0;i<row.size();i++)
    data_[i].push_back(row[i]);
}

void stats_table::load_file(istream& file,int skip,int max)
{
  // Read in heaers from file
  names_ = read_header(file);

  data_.resize(names_.size());

  // Read in data
  int line_number=0;
  string line;
  while(getline(file,line)) 
  {
    line_number++;

    if (line_number <= skip) continue;

    vector<double> v = split<double>(line,'\t');

    if (v.size() != n_columns())
      throw myexception()<<"Found "<<v.size()<<"/"<<n_columns()<<" values on line "<<line_number<<".";
    
    add_row(v);

    if (max != -1 and n_rows() >= max)
      break;
  }


}

stats_table::stats_table(istream& file,int skip,int max)
{
  load_file(file,skip,max);
}
