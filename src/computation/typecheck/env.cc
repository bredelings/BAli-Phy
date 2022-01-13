#include "env.H"
#include "util/string/join.H"

using std::string;
using std::vector;

string print(const value_env& env)
{
    std::ostringstream oss;
    vector<string> ss;
    for(auto& [value,type]: env)
    {
        ss.push_back(value+" :: "+type.print());
    }
    return "{ " + join(ss, "; ") + " }";
}

void add_prefer_right(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
            e1 = e1.insert({x,t});
}

value_env plus_prefer_right(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_prefer_right(e3,e2);
    return e3;
}

void add_no_overlap(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1 = e1.insert({x,t});
        }
}

value_env plus_no_overlap(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
}

value_env& operator+=(value_env& env1, const value_env& env2)
{
    add_no_overlap(env1, env2);
    return env1;
}

value_env operator+(const value_env& env1, const value_env& env2)
{
    return plus_no_overlap(env1, env2);
}

void add_no_overlap(type_con_env& e1, const type_con_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1.insert({x,t});
        }
}

type_con_env plus_no_overlap(const type_con_env& e1, const type_con_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
}

