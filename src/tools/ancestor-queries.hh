#ifndef ANCESTOR_QUERIES_HH
#define ANCESTOR_QUERIES_HH

#include <map>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
#include <boost/dynamic_bitset.hpp>
#include "alignment/alignment.hh"
#include "tree/sequencetree.hh"

std::optional<SequenceTree> get_node_queries(const std::optional<std::string>& filename,
                                            const std::vector<std::string>& leaf_names);
std::optional<std::vector<std::pair<std::string,boost::dynamic_bitset<>>>> get_branch_queries(
    const std::optional<std::string>& filename, const std::vector<std::string>& leaf_names);
std::map<int,int> find_and_name_nodes(const SequenceTree&, const SequenceTree&);
std::map<int,int> find_and_name_nodes(
    const SequenceTree&, const std::vector<std::pair<std::string,boost::dynamic_bitset<>>>&);
std::ostream& write_alignment_row(std::ostream&, const std::string& name, const alignment&, int row);

#endif
