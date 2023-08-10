#ifndef HASKELL_IDS
#define HASKELL_IDS

#include <string>
#include <vector>

std::string tuple_name(int n);
int tuple_arity(const std::string&);
bool is_tuple_name(const std::string&);

bool is_haskell_varid(const std::string& s);
bool is_haskell_conid(const std::string& s);
bool is_haskell_id(const std::string& s);
bool is_haskell_varsym(const std::string& s);
bool is_haskell_consym(const std::string& s);
bool is_haskell_normal_con_name(const std::string& s);
bool is_haskell_builtin_con_name(const std::string& s);
bool is_haskell_builtin_type_name(const std::string& s);
bool is_haskell_sym(const std::string& s);
bool is_haskell_uqsym(const std::string& s);
bool is_haskell_qsym(const std::string& s);
bool is_haskell_qid(const std::string& s);

bool is_haskell_var_name(const std::string& s);
bool is_haskell_con_name(const std::string& s);
bool is_haskell_module_name(const std::string& s);

bool is_qualified_symbol(const std::string&);

bool is_qualified_by_module(const std::string&, const std::string&);
bool is_local_symbol(const std::string&, const std::string&);

std::string get_module_name(const std::string&);
std::string get_unqualified_name(const std::string&);

std::vector<std::string> get_haskell_identifier_path(const std::string& s);

bool is_haskell_symbol_char(char c);
bool is_haskell_id_char(char c);

#endif
