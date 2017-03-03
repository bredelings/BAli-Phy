#ifndef AST_NODE_H
#define AST_NODE_H

#include "object.H"
#include <string>

struct AST_node: public Object
{
    AST_node* clone() const {return new AST_node(*this);}
    std::string type;
    std::string value;
    tribool compare(const Object& o) const;

    std::string print() const;

    AST_node(const std::string&);
    AST_node(const std::string& t, const std::string& v);
};


#endif
