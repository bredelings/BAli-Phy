#include "logger.H"
#include "computation/haskell/generated.H"

using std::string;
using std::vector;

namespace HsG = Haskell::Generated;

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

// Reports whether a logger tree contains a leaf of the requested kind.
bool has_loggers(const Loggers& loggers, LogValueKind kind)
{
    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
        {
            if (has_loggers(lsub->loggers, kind))
                return true;
        }
        else if (auto lvalue = l.as<LogValue>(); lvalue and lvalue->kind == kind)
            return true;
        else if (l.as<LogContextFields>() and kind == LogValueKind::context)
            return true;
    }
    return false;
}

// Emits both logger projections while binding each nested LoggerValues expression once.
Hs::Exp generate_logger_values(Hs::Stmts& code, const Loggers& loggers)
{
    vector<Hs::Exp> parameters;
    vector<Hs::Exp> context;

    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
        {
            auto log_x = lsub->log_var;
            HsG::Let(code, log_x, generate_logger_values(code, lsub->loggers));

            auto prefix = Hs::Literal(Hs::String{lsub->prefix});
            if (has_loggers(lsub->loggers, LogValueKind::parameter))
                parameters.push_back(HsG::Apply(Hs::Var("%>%"),
                                                {prefix,
                                                 HsG::Apply(Hs::Var("parameterLogValues"), {log_x})}));
            if (has_loggers(lsub->loggers, LogValueKind::context))
                context.push_back(HsG::Apply(Hs::Var("%>!"),
                                             {prefix,
                                              HsG::Apply(Hs::Var("contextLogValues"), {log_x})}));
        }
        else if (auto lvalue = l.as<LogValue>())
        {
            auto name = Hs::Literal(Hs::String{lvalue->name});
            if (lvalue->kind == LogValueKind::parameter)
                parameters.push_back(HsG::Apply(Hs::Var("%=%"), {name, lvalue->value}));
            else
                context.push_back(HsG::Apply(Hs::Var("%=!"), {name, lvalue->value}));
        }
        else if (auto fields = l.as<LogContextFields>())
        {
            auto prefix = Hs::Literal(Hs::String{fields->prefix});
            context.push_back(HsG::Apply(Hs::Var("prefixContextFields"), {prefix, fields->value}));
        }
        else
            std::abort();
    }

    auto context_fields = HsG::Apply(Hs::Var("contextFields"), {HsG::List(context)});
    return HsG::Apply(Hs::Var("LoggerValues"), {HsG::List(parameters), context_fields});
}
