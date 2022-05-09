#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

Hs::FunDecl dictionary_extractor(const string& name, int i, int N)
{
    // Maybe we should emit the case directly, instead of relying on desugaring?

    // FIXME: We might need to put types on "extractor" and "field".

    Hs::Var field({noloc,"field"});

    // extractor (_,field,_,_) = field;
    Hs::Var extractor({noloc, name});

    // (_,field,_,_)
    vector<Hs::Pattern> pats(N, Hs::WildcardPattern());
    pats[i] = field;
    Hs::Pattern pattern = Hs::tuple(pats);

    // (_,field,_,_) -> field
    Hs::MRule rule{{pattern}, Hs::SimpleRHS({noloc, field})};
    Hs::Match match{{rule}};

    return Hs::FunDecl(extractor, match);
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of (_,_,method,_,_) -> method }
//                       = { made-up-name = \dict -> case dict of (superdict,_,_,_,_) -> superdict }

tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
typechecker_state::infer_type_for_class(const Hs::ClassDecl& class_decl)
{
    kindchecker_state K(tce);

    class_info cinfo;
    cinfo.type_vars = class_decl.type_vars;
    cinfo.name = class_decl.name;
    cinfo.context = class_decl.context;

    // 1. Bind type parameters for class
    K. push_type_var_scope();

    // 1a. Look up kind for this data type.
    auto class_kind = K.kind_for_type_con(cinfo.name);

    // 1b. Record the kind for each type variable.
    for(auto& tv: class_decl.type_vars)
        K.bind_type_var(tv, *tv.kind);

    // 2. construct the constraint that represent the class
    Hs::Type class_constraint = Hs::TypeCon(Unlocated(class_decl.name)); // put class_kind into TypeCon?
    for(auto& tv: class_decl.type_vars)
        class_constraint = Hs::TypeApp(class_constraint, tv);

    // 3. make global types for class methods
    global_value_env gve;
    if (class_decl.binds)
    {
        // Add class methods to GVE
        for(auto& [qname, type]: unloc(*class_decl.binds).signatures)
        {
            auto method_type = K.kind_and_type_check_type(type);

            // forall a. C a => method_type
            method_type = Hs::add_constraints({class_constraint}, method_type);
            method_type = Hs::add_forall_vars(class_decl.type_vars, method_type);

            gve = gve.insert({qname, method_type});
            cinfo.members = cinfo.members.insert({get_unqualified_name(qname), method_type});
        }

        // Get names and types for default methods
        for(auto& decls: unloc(*class_decl.binds))
            for(auto& decl: decls)
            {
                auto& FD = decl.as_<Hs::FunDecl>();
                auto& name = unloc(FD.v.name);
                auto dm = fresh_var("dm"+name, true);
                cinfo.default_methods.insert({name, dm});

                if (not cinfo.members.count(name))
                    throw myexception()<<"In class '"<<cinfo.name<<"', default value for undefined method '"<<name<<"'";
                auto type = cinfo.members.at(name);

                gve = gve.insert({unloc(dm.name), type});
            }
    }

    K.pop_type_var_scope();

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    // 4. Make global types for superclass extractors
    global_instance_env gie;
    for(auto& superclass_constraint: cinfo.context.constraints)
    {
        string cname1 = get_class_name_from_constraint(superclass_constraint);
        string cname2 = get_class_name_from_constraint(class_constraint);
        string extractor_name = cname1+"From"+cname2;

        auto get_dict = fresh_var(extractor_name, true);
        // Should this be a function arrow?
        Hs::Type type = Hs::add_constraints({class_constraint}, superclass_constraint);
        type = apply_current_subst(type);
        // Maybe intersect the forall_vars with USED vars?
        type = add_forall_vars(class_decl.type_vars, type);
        gie = gie.insert({unloc(get_dict.name), type});

        // Is this right???
        cinfo.fields.push_back({unloc(get_dict.name),type});
    }
    for(auto& [name,type]: cinfo.members)
        cinfo.fields.push_back({mod_name+"."+name,type});

    // 5. Define superclass extractors and member function extractors
    Hs::Decls decls;

    vector<Hs::Type> types;
    for(auto& [name,type]: cinfo.fields)
        types.push_back(type);
    Hs::Type dict_type = Hs::tuple_type(types);

    int i = 0;
    for(auto& [name,type]: cinfo.fields)
    {
        // body = \dict -> case dict of (_,field,_,_) -> field

        // dict
        Hs::Var dict({noloc,"dict"});
        // field
        Hs::Var field({noloc,"field"});

        // (_,field,_,_)
        vector<Hs::Pattern> pats(cinfo.fields.size(), Hs::WildcardPattern());
        pats[i] = field;

        // (,field,_,_) -> field
        Hs::Alt alt{Hs::tuple(pats),Hs::SimpleRHS({noloc,field})};

        // case dict of (_,field,_,_) -> field
        Hs::CaseExp case_exp(dict,Hs::Alts({{noloc,alt}}));

        // dict -> case dict of (_,field,_,_) -> field
        Hs::MRule rule{{dict},Hs::SimpleRHS({noloc,case_exp})};
        Hs::Match m{{rule}};

        // f = dict -> case dict of (_,field,_,_) -> field
        Hs::Var f({noloc,name});
        decls.push_back( Hs::FunDecl(f,m) );

        i++;
    }

    return {gve,gie,cinfo,decls};
}

tuple<global_value_env, global_instance_env, class_env, Hs::Binds> typechecker_state::infer_type_for_classes(const Hs::Decls& decls)
{
    global_value_env gve;
    global_instance_env gie;
    class_env ce;
    Hs::Binds binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [gve1, gie1, class_info, class_decls] = infer_type_for_class(*c);

        gve += gve1;
        gie += gie1;
        ce.insert({class_info.name, class_info});
        binds.push_back(class_decls);
    }

    return {gve, gie, ce, binds};
}

