/*
  Copyright (C) 2019 Benjamin Redelings

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

#include "newick-tokenizer.H"
#include "util/string/pred.H"
#include "util/myexception.H"

using std::vector;
using std::string;


 // for split( ), find_index( ), join( ), bad_mapping
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
		start = i;
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
		    if (i+1 < s.size() and s[i+1] == '\'')
			i++;
		    else
		    {
			word += s.substr(start, i+1-start);
			start = i+1;
			in_quote = false;
		    }
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

