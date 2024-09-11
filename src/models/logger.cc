#include "logger.H"
#include "computation/expression/list.H"            // for get_list( )

using std::string;
using std::vector;

void simplify(Loggers& loggers)
{
    // 1. Second, determine if we have subloggers where we can lift children out and append them to the prefix.
    std::vector<bool> lift_children(loggers.size(), false);
    for(int i=0;i<loggers.size();i++)
    {
        auto& l = loggers[i];
        if (auto lsub = l.as<LogSub>())
        {
            lift_children[i] = true;
            for(auto& ll: lsub->loggers)
            {
                if (auto llsub = ll.as<LogSub>())
                {
                    if (llsub->prefix.size() > 0 and llsub->prefix[0] == '[')
                        continue;
                }
                lift_children[i] = false;
                break;
            }
        }
    }

    /// 2. Move the children
    Loggers loggers3;
    for(int i=0; i<loggers.size(); i++)
    {
        auto& l = loggers[i];
        if (not lift_children[i])
            loggers3.push_back(std::move(l));
        else
        {
            auto lsub = l.as<LogSub>();
            for(auto& ll: lsub->loggers)
            {
                const auto& llsub = ll.as<LogSub>();
                llsub->prefix = lsub->prefix + llsub->prefix;
                loggers3.push_back(std::move(ll));
            }
        }
    }
    std::swap(loggers, loggers3);

    // 3. First we simplify all the level below this one.
    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
            simplify(lsub->loggers);
    }

    // 4. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name in a sibling.
    // We therefore count which names at these levels occur twice and avoid them.

    // NOTE: In theory we could have subloggers with internal names/prefixes that overlap with the
    // external prefix of another logger.  But if that logger is going away, then its external
    // prefix will disappear and the conflict isn't real.  However, we do prevent merging
    // subloggers that have names that conflict with the (external) prefix of another logger.
    // EXAMPLE: If we have a situation like {I1/S1, S2/I1} then this approach won't simplify to {S1,I1}

    std::multiset<string> names;
    for(auto& l: loggers)
    {
        names.insert(l->get_name());
        if (auto lsub = l.as<LogSub>())
            for(auto& ll: lsub->loggers)
                names.insert(ll->get_name());
    }

    // 5. If none of the names in an entry occur twice, then we can move all then
    //    names in that entry up to the top level.
    vector<bool> move_children(loggers.size());
    for(int i=0; i<loggers.size(); i++)
    {
        auto l = loggers[i].as<LogSub>();
        if (not l) continue;

        bool ok = true;
        for(auto& ll: l->loggers)
        {
            if (names.count(ll->get_name()) > 1)
            {
                ok = false;
                break;
            }
        }
        if (ok)
            move_children[i] = true;
    }

    /// 6. Move the children
    Loggers loggers2;
    for(int i=0; i<loggers.size(); i++)
    {
        auto& l = loggers[i];
        if (not move_children[i])
            loggers2.push_back(std::move(l));
        else
        {
            for(auto& ll: l.as<LogSub>()->loggers)
                loggers2.push_back(std::move(ll));
        }
    }
    std::swap(loggers, loggers2);
}


expression_ref generate_loggers(do_block& code, const Loggers& loggers)
{
    vector<expression_ref> simple_loggers;
    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
        {
            auto log_x = lsub->log_var;
            auto logger_list = generate_loggers(code,lsub->loggers);
            code.let(log_x,logger_list);
            simple_loggers.push_back({var("%>%"),String(lsub->prefix),log_x});
        }
        else if (auto lvalue = l.as<LogValue>())
            simple_loggers.push_back({var("%=%"),String(lvalue->name),lvalue->value});
        else
            std::abort();
    }
    return get_list(simple_loggers);
}


