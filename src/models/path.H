#ifndef MODEL_PATH_H
#define MODEL_PATH_H

#include <string>
#include <vector>


std::string model_extend_path(const std::string&,const std::string&);
std::string model_path(const std::vector<std::string>&);
std::vector<std::string> model_split_path(const std::string&);

std::vector<std::string> short_parameter_names(const std::vector<std::string>& names);


#endif
