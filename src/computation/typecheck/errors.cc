#include "typecheck.H"
#include "kindcheck.H"

#include "util/text.H"      // for ANSI colors
#include "util/set.H"       // for add( , )
#include "util/variant.H"   // for to< >()
#include "haskell/ids.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

string print_unqualified_id(const string& s)
{
    auto s2 = get_unqualified_name(s);
    if (is_haskell_sym(s2))
        s2 = "("+s2+")";
    return s2;
}

string print_unqualified_id(const Located<string>& ls)
{
    return print_unqualified_id(unloc(ls));
}

void TypeChecker::check_wanteds(const WantedConstraints& wanteds, const TypeCheckerContext& context)
{
    for(auto& wanted: wanteds.simple)
    {
        std::optional<yy::location> loc;
        Note e;
        if (auto occ = to<OccurrenceOrigin>(wanted.origin))
        {
            e<<"Could not derive `"<<bold_green(print_unqualified(wanted.pred))<<ANSI::bold<<"`";
            e<<" arising from a use of `"<<cyan(print_unqualified_id(occ->name))<<ANSI::bold<<"`";
            if (occ->name.loc)
            {
                loc = occ->name.loc;
                e<<" at "<<(*loc);
            }
        }
        else if (auto uorig = to<UnifyOrigin>(wanted.origin))
        {
            e<<"Expected `"<<bold_green(print_unqualified(uorig->t2))<<ANSI::bold<<"` but got `"<<bold_green(print_unqualified(uorig->t1))<<ANSI::bold<<"`";
        }
        else if (auto sorig = to<StringOrigin>(wanted.origin))
        {
            e<<sorig->msg;
        }
        else if (auto app = to<AppOrigin>(wanted.origin))
        {
            e<<"Applying "<<(app->arg_index+1)<<" arguments to function "<<app->head.print()<<", but it only takes "<<app->arg_index<<"!";
            loc = app->loc;
        }
        else if (auto lsec = to<LeftSectionOrigin>(wanted.origin))
            e<<"In left section, "<<lsec->op<<" is not a function!";
        else
            e<<"Could not derive `"<<bold_green(print_unqualified(wanted.pred))<<ANSI::bold<<"`";

        record_error(loc, context, e);
    }

    for(auto& implic: wanteds.implications)
        check_wanteds(implic->wanteds,implic->context);
}

