#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <set>
#include "computation/object.H"

class expression_ref;

// a variable expression
struct type_var: public Object
{
    int index;

    std::string name;

    bool operator==(const type_var&) const;
    bool operator==(const Object& o) const;
    type_var* clone() const {return new type_var(*this);}
    std::string print() const;
  
    bool operator<(const type_var&) const;

    type_constant type() const {return type_var_type;}

    type_var(int i):index(i) {}
    type_var(const std::string& s):type_var(s,0) {}
    type_var(const std::string& s,int i):index(i),name(s) {assert(name != "_");}
};

bool is_type_var(const expression_ref& E);

// a type constant
struct type_con: public Object
{
    // object_ptr<id_t> id;
    int index;

    std::string name;

    // object_ptr<kind_t> k;
    
    bool operator==(const type_con&) const;
    bool operator==(const Object& o) const;
    type_con* clone() const {return new type_con(*this);}
    std::string print() const;
  
    bool operator<(const type_con&) const;

    type_constant type() const {return type_con_type;}

    type_con(int i):index(i) {}
    type_con(const std::string& s):type_con(s,0) {}
    type_con(const std::string& s,int i):index(i),name(s) {assert(name != "_");}
};

bool is_type_con(const expression_ref& E);

// type apply

struct type_apply_node: public Object
{
    type_apply_node* clone() const {return new type_apply_node(*this);}
    bool operator==(const Object& o) const;
    type_constant type() const {return type_apply_node_type;}
    std::string print() const;
};

bool is_type_apply_node(const expression_ref& E);

expression_ref type_apply(const expression_ref& t1, const expression_ref& t2);

bool is_type_apply(const expression_ref& E);

// type forall

struct type_forall_node: public Object
{
    type_forall_node* clone() const {return new type_forall_node(*this);}
    bool operator==(const Object& o) const;
    type_constant type() const {return type_forall_node_type;}
    std::string print() const;
};

bool is_type_forall_node(const expression_ref& E);

expression_ref type_forall(const expression_ref& x, const expression_ref& E);

bool is_type_forall(const expression_ref& E);

bool is_type_con_expression(const expression_ref& E);

expression_ref expand_type(const expression_ref& E);

// function types

type_con function_type_con_node();

bool is_function_type_con_node(const expression_ref& E);

expression_ref function_type(const expression_ref& t1, const expression_ref& t2);

bool is_function_type(const expression_ref& E);

bool is_type(const expression_ref& E);


// free type variables

void get_free_type_variables(const expression_ref& E, std::set<type_var>&);
std::set<type_var> free_type_variables(const expression_ref& E);

namespace std
{
    template <>
    class hash < type_var >{
    public :
        size_t operator()(const type_var &x) const
        {
            size_t h =   std::hash<int>()(x.index) ^ std::hash<std::string>()(x.name);
            return  h ;
        }
    };
}

#endif
