/*
   Copyright (C) 2004-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

/**
 * @file util.C
 *
 * @brief This file contains a number of utility routines.
 *
 */

#include "util.H"

using std::vector;
using std::string;
using std::pair;

#include <iostream>
using std::cerr;
using std::endl;

int log_verbose = 0;

/// \brief Compute the number of 'true' elements in bitvector \a v.
///
/// \param v The bitvector.
/// 
int n_elements(const vector<bool>& v) {
  int count = 0;
  for(int i=0;i<v.size() ;i++)  
    if (v[i]) count++;
  return count;
}


/// \brief Combine vector of strings \a v into one string, separating entries with the character \a c.
///
/// \param v The strings to combine.
/// \param c The separator.
///
string join(const vector<string>& v,char c) {
  string s;
  if (v.size())
    s = v[0];
  for(int i=1;i<v.size();i++) {
    s += string(1,c);
    s += v[i];
  }
  return s;
}

/// \brief Combine vector of strings \a v into one string, separating entries with the string \a token.
///
/// \param v The strings to combine.
/// \param token The separator.
///
string join(const vector<string>& v,const string& token) {
  string s;
  if (v.size())
    s = v[0];
  for(int i=1;i<v.size();i++) {
    s += token;
    s += v[i];
  }
  return s;
}


/// \brief Divide a string \a s into several strings by break it at the character \a c
///
/// \param s The string to split.
/// \param c The separator.
///
vector<string> split(const string& s, char c) 
{
  vector<string> strings;
  int length=0;
  for(int i=0;i<s.size();i++)
    if (s[i] == c) {
      strings.push_back(s.substr(i-length,length));
      length = 0;
    }
    else
      length++;
  
  strings.push_back(s.substr(s.size()-length,length));
  return strings;
}

/// \brief Divide a string \a s into several strings by break it at the string \a token.
///
/// \param s The string to split.
/// \param token The separator.
///
vector<string> split(const string& s, const string& token)
{
  vector<string> strings;

  int p1 = 0;
  do {
    int p2 = s.find(token,p1);
    if (p2 == -1)
    {
      strings.push_back(s.substr(p1));
      break;
    }
    else 
    {
      assert(p2 < s.size());

      strings.push_back(s.substr(p1,p2-p1));
      p1 = p2 + token.size();

      assert(p1 <= s.size());
    }
  } while (true);
  return strings;
}

/// \brief Remove the character \a c from the string \a s.
///
/// \param s The string to strip.
/// \param c The character to remove.
/// \return the stripped string.
///
string strip(const string& s,char c) {
  string s2;
  for(int i=0;i<s.size();i++)
    if (s[i] != c)
      s2 += s[i];

  return s2;
}


/// \brief Remove all characters in string \a chars from the string \a s.
///
/// \param s The string to strip.
/// \param chars The string containing characters to remove.
/// \return the stripped string.
///
string strip(const string& s,const string& chars) {
  string s2;
  for(int i=0;i<s.size();i++) {
    bool found = false;
    for(int j=0;j<chars.size() and not found;j++) {
      if (s[i] == chars[j])
	found = true;
    }
    if (not found)
      s2 += s[i];
  }

  return s2;
}


vector<int> invert(const vector<int>& mapping) 
{
  vector<int> imapping(mapping.size(),-1);

  for(int i=0;i<imapping.size();i++)
    imapping[mapping[i]] = i;

  return imapping;
}

vector<int> compose(const vector<int>& mapping1,const vector<int>& mapping2) {
  assert(mapping1.size() == mapping2.size());

  vector<int> mapping(mapping1.size());

  for(int i=0;i<mapping.size();i++)
    mapping[i] = mapping2[mapping1[i]];

  return mapping;
}

/// \brief Check if \a mapping[i] == i
///
/// \param mapping The mapping.
///
bool is_identity(const std::vector<int>& mapping)
{
  for(int i=0;i<mapping.size();i++)
    if (mapping[i] != i)
      return false;
  return true;
}

/// \brief Check if a string \a s contains a character \a c.
///
/// \param s The string.
/// \param c The character.
/// \return true if \a s contains \a c.
/// 
bool contains_char(const string& s,char c) {
  for(int i=0;i<s.size();i++)
    if (s[i] == c)
      return true;
  return false;
}

bool get_word(string& word, int& i, const string& s,
	      const string& delimiters,const string& whitespace) 
{
  vector<string> comments;
  return get_word(word, i, comments, s, delimiters, whitespace);
}

bool get_word(string& word, int& i, vector<string>& comments,const string& s,
	      const string& delimiters,const string& whitespace) 
{
  int start = -1;
  int start_comment = -1;
  bool in_word = false;
  bool in_quote = false;
  bool in_comment = false;
  word.clear();
  comments.clear();

  for(;i<s.size();i++)
  {
    char c = s[i];

    if (not in_word)
    {
      if (in_comment)
      {
	if (c == ']') {
	  comments.back() = s.substr(start_comment, i-start_comment);
	  in_comment = false;
	}
	continue;
      }

      // Don't have to handle in_quote here -> can't happen.

      if (contains_char(whitespace, c)) continue;

      // If we are parsing a delimiter, then take the char, advance the position, and return true.
      if (contains_char(delimiters, c))
      {
	word = s.substr(i,1);
	i++;
	return true;
      }

      if (c == '\'')
      {
	in_word = true;
	in_quote = true;
	start = i+1;
	continue;
      }

      if (not in_comment and c == '[')
      {
	in_comment = true;
	start_comment = i+1;
	comments.push_back(string());
	continue;
      }

      in_word = true;
      start = i;
    }
    else
    {
      if (in_comment)
      {
	if (c == ']') {
	  comments.back() = s.substr(start_comment, i-start_comment);
	  in_comment = false;
	  start = i+1;
	}
	continue;
      }

      if (in_quote)
      {
	if (c == '\'')
	{
	  word += s.substr(start, i-start);
	  start = i+1;
	  in_quote = false;
	}
	continue;
      }

      if (c == '\'')
      {
	word += s.substr(start, i-start);
	start = i+1;
	in_quote = true;
	continue;
      }

      // Keep the position here, but don't add this char to the current word.
      if (contains_char(whitespace, c)) break;

      // Keep the position here, but don't add this char to the current word.
      if (contains_char(delimiters, c)) break;

      if (not in_comment and c == '[')
      {
	word += s.substr(start, i-start);
	in_comment = true;
	start_comment = i+1;
	comments.push_back(string());
	continue;
      }

    }
  }

  if (in_quote)
    throw myexception()<<"Unterminated quote parsing tree";

  if (in_comment)
    throw myexception()<<"Unterminated comment parsing tree";

  if (not in_word) return false;

  if (start != -1)
    word += s.substr(start,i-start);

  return true;
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
  s = s.substr(0,loc);

  if (loc == -1) 
    return vector<string>();
  
  return split(args,',');
}

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

void check_supplied_filenames(int n,const vector<string>& files, bool exact)
{
  if (files.size() == n)
    return;

  if (files.size() < n)
    throw myexception()<<"Wanted "<<n<<" filenames, but got only "<<files.size()<<".";
  if (exact and files.size() > n)
    cerr<<"Warning: ignoring "<<files.size()-n<<" extra filenames."<<endl;
}

#ifdef __GNUC__
#include <cxxabi.h>
string demangle(const string& s)
{
  int     status;
  char* result_str = abi::__cxa_demangle(s.c_str(), 0, 0, &status);

  string result = result_str;
  free(result_str);
  return result;
}
#else
string demangle(const string& s)
{
  return s;
}
#endif

string sanitize_string(const string& s1)
{
  string s2;
  for(int i=0;i<s1.size();i++) {
    char c = s1[i];
    if (c >=32 and c<= 126)
      s2 += c;
    else
      s2 = s2 + "[" + convertToString((int)c) + "]";
  }
  return s2;
}
