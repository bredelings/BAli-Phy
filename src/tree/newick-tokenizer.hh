#ifndef TREE_NEWICK_TOKENIZER_H
#define TREE_NEWICK_TOKENIZER_H

#include <string>
#include <vector>

/// get the next word starting at position i, return true if not done.
bool get_word(std::string& word, int& i,const std::string& s,
	      const std::string& delimiters,const std::string& whitespace);

bool get_word(std::string& word, int& i, std::vector<std::string>& comments, const std::string& s,
	      const std::string& delimiters,const std::string& whitespace);

#endif
