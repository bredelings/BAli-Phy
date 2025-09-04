/*
  Copyright (C) 2004-2010 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_FENV_H
extern "C" {
#include "fenv.h"
}
#endif

#include "util/time.H"

#ifdef HAVE_MPI
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi = boost::mpi;
#endif

#include <cmath>
#include <iostream>
#include <sstream>
#include <new>
#include <map>
#include <filesystem>
#include <fmt/chrono.h>
#include <chrono>

#include <boost/program_options.hpp>

#include "substitution/likelihood.H"
#include "util/myexception.H"
#include "util/rng.H"
#include "util/mapping.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/log-level.H"
#include "util/set.H"
#include "util/text.H"
#include "models/parameters.H"
#include "models/rules.H"
#include "models/compile.H"
#include "math/pow2.H"
#include "version.H"
#include "computation/module.H"
#include "computation/optimization/inliner.H" // for simple_size( )
#include "computation/program.H"
#include "models/A-T-prog.H" // for gen_model_program( )

#include "computation/haskell/ids.H"
#include "computation/typecheck/tidy.H"  // for TidyState.print( )

#include "A-T-model.H"
#include "files.H"
#include "paths.H"
#include "io.H"
#include "system.H"
#include "cmd_line.H"
#include "computation/expression/expression.H" // for parse_object( )
#include "computation/loader.H"

namespace fs = std::filesystem;
namespace chrono = std::chrono;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ifstream;
using std::ofstream;
using std::ostream;
using std::string;
using std::vector;
using std::map;
using std::set;
using std::optional;

using std::shared_ptr;

/* 
 * 1. Add a PRANK-like initial algorithm.
 * 2. Add some kind of constraint.
 * 3. Improve the method for proposing new SPR attachment sites.
 *  3a. Can we walk along the tree making characters present?
 */

void report_mem() {
#if !defined(_MSC_VER) && !defined(__MINGW32__)
/*
  struct rusage usage;
  std::cerr<<getrusage(RUSAGE_SELF,&usage);
  std::cerr<<"Maximum resident set size: "<<usage.ru_maxrss<<"\n";
  std::cerr<<"Integral shared memory size: "<<usage.ru_ixrss<<"\n";
  std::cerr<<"Integral unshared data size: "<<usage.ru_idrss<<"\n";
  std::cerr<<"Integral unshared stack size: "<<usage.ru_isrss<<"\n";
  std::cerr<<"Number of swaps: "<<usage.ru_nswap<<"\n";
  std::cerr.flush();
*/
  int pid = getpid();
  std::ostringstream cmd;
  cmd<<"cat /proc/"<<pid<<"/status | grep Vm 1>&2";
  system(cmd.str().c_str());
  std::cerr.flush();
#endif
}

#ifdef DEBUG_MEMORY
void * operator new(size_t sz)
    printf("new called, sz = %d\n",sz);
    return malloc(sz); 
}

void operator delete(void * p)
    printf("delete called, content = %d\n",(*(int*)p));
    free(p); 
}
#endif

/// Initialize the default random number generator and return the seed
unsigned long init_rng_and_get_seed(const variables_map& args)
{
    unsigned long seed = 0;
    if (args.count("seed")) {
        seed = args["seed"].as<unsigned long>();
        myrand_init(seed);
    }
    else
        seed = myrand_init();

    return seed;
}

chrono::system_clock::time_point start_time = chrono::system_clock::now();
auto start_time_t = chrono::system_clock::to_time_t(start_time);

auto start_work_time = chrono::system_clock::now();
auto start_work_time_t = chrono::system_clock::to_time_t(start_work_time);
auto start_work_cpu_time = total_cpu_time();

void show_ending_messages()
{
    using namespace chrono;

    if (log_verbose >= 2) {

        extern long total_reductions;
        extern long total_changeable_eval;
        extern long total_changeable_eval_with_result;
        extern long total_changeable_eval_with_call;
        extern long total_changeable_reductions;
        extern long total_invalid_control_flow;
        extern long total_valid_control_flow;

        extern long total_reductions2;
        extern long total_changeable_eval2;
        extern long total_changeable_eval_with_result2;
        extern long total_changeable_eval_with_call2;
        extern long total_changeable_reductions2;

        extern long total_reg_allocations;
//        extern long total_comp_allocations;
        extern long total_step_allocations;
        extern long total_destroy_token;
        extern long total_release_knuckle;
        extern long total_create_context1;
        extern long total_create_context2;
        extern long total_tokens;
        extern long total_reroot;
        extern long total_reroot_one;
        extern long total_set_reg_value;
        extern long total_get_reg_value;
        extern long total_get_reg_value_non_const;
        extern long total_get_reg_value_non_const_with_result;
        extern long total_invalidate;
        extern long total_steps_invalidated;
        extern long total_results_invalidated;
//        extern long total_forces_invalidated;
        extern long total_steps_scanned;
        extern long total_results_scanned;
//        extern long total_forces_scanned;
        extern long total_steps_pivoted;
        extern long total_results_pivoted;
//        extern long total_forces_pivoted;
        extern long total_force_counts_pivoted;
        extern long total_context_pr;
        extern long total_gc;
        extern long total_regs;
        extern long total_steps;

        extern long total_case_op;
        extern long total_let_op;
        extern long total_index_op;
        if (total_reductions > 0)
        {
            cout<<"Program evaluation:\n";
            cout<<"  total changeable evals         = "<<total_changeable_eval2<<endl;
            cout<<"    with result                  = "<<total_changeable_eval_with_result2<<endl;
            cout<<"    with call but not result     = "<<total_changeable_eval_with_call2<<endl;
            cout<<"  total reduction steps          = "<<total_reductions2<<endl;
            cout<<"    changeable reduction steps   = "<<total_changeable_reductions2<<endl;
            cout<<"    unchangeable reduction steps = "<<total_reductions2-total_changeable_reductions2<<endl;
            cout<<"\n";
            cout<<"Side evaluation:\n";
            cout<<"  total changeable evals         = "<<total_changeable_eval<<endl;
            cout<<"    with result                  = "<<total_changeable_eval_with_result<<endl;
            cout<<"    with call but not result     = "<<total_changeable_eval_with_call<<endl;
            cout<<"  total reduction steps          = "<<total_reductions<<endl;
            cout<<"    changeable reduction steps   = "<<total_changeable_reductions<<endl;
            cout<<"    unchangeable reduction steps = "<<total_reductions-total_changeable_reductions<<endl;
            cout<<"\n";
            cout<<"  op:case                      = "<<total_case_op<<endl;
            cout<<"  op:let                       = "<<total_let_op<<endl;
            cout<<"  op:index                     = "<<total_index_op<<endl;
            cout<<"\ntotal garbage collection runs  = "<<total_gc<<endl;
            cout<<"total register allocations     = "<<total_reg_allocations<<endl;
            cout<<"total step allocations         = "<<total_step_allocations<<endl;
            cout<<"total regs                     = "<<total_regs<<endl;
            cout<<"total steps                    = "<<total_steps<<endl;
            cout<<"\ntotal reroot operations        = "<<total_reroot<<endl;
            cout<<"  total atomic reroot          = "<<total_reroot_one<<endl;
            cout<<"  total steps pivoted          = "<<total_steps_pivoted<<endl;
            cout<<"  total results pivoted        = "<<total_results_pivoted<<endl;
            cout<<"  total force counts pivoted   = "<<total_force_counts_pivoted<<endl;
            cout<<"\n";
            cout<<"total invalidations            = "<<total_invalidate<<endl;
            cout<<"  total invalid flow           = "<<total_invalid_control_flow<<endl;
            cout<<"  total valid flow             = "<<total_valid_control_flow<<endl;
            cout<<"  total steps invalidated      = "<<total_steps_invalidated<<endl;
            cout<<"  total results invalidated    = "<<total_results_invalidated<<endl;
            cout<<"  total steps scanned          = "<<total_steps_scanned<<endl;
            cout<<"  total results scanned        = "<<total_results_scanned<<endl;
            cout<<"total tokens                   = "<<total_tokens<<endl;
            cout<<"total tokens destroyed         = "<<total_destroy_token<<endl;
            cout<<"  total knuckles destroyed     = "<<total_release_knuckle<<endl;
            cout<<"total create context           = "<<total_create_context1+total_create_context2<<endl;
            cout<<"  operator=                    = "<<total_create_context1<<endl;
            cout<<"  copy constructor             = "<<total_create_context2<<endl;
            cout<<"\ntotal values set               = "<<total_set_reg_value<<endl;
            cout<<"total values gotten            = "<<total_get_reg_value<<endl;
            cout<<"total values gotten variable   = "<<total_get_reg_value_non_const<<endl;
            cout<<"total values gotten w/ result  = "<<total_get_reg_value_non_const_with_result<<endl;
            cout<<"total context probability      = "<<total_context_pr<<endl;
        }
    }
    auto end_time = system_clock::now();
    auto end_time_t = system_clock::to_time_t(end_time);
    auto end_cpu_time = total_cpu_time();

    if (log_verbose >= 1)
    {
        cout<<endl;
        cout<<"Setup:"<<endl;
        cout<<"  start: "<<fmt::format("{:%c}", *std::localtime(&start_time_t))<<endl;
	cout<<"    end: "<<fmt::format("{:%c}", *std::localtime(&start_work_time_t))<<endl;
        cout<<"  total (elapsed) time: "<<duration_string( start_work_time-start_time )<<endl;
        cout<<"  total (CPU) time: "<<duration_string( start_work_cpu_time )<<endl;
    }

    if (end_time - start_work_time > seconds(2) or log_verbose >= 1)
    {
        cout<<endl;
        cout<<"Work:"<<endl;
        cout<<"  start: "<<fmt::format("{:%c}", *std::localtime(&start_work_time_t))<<endl;
        cout<<"    end: "<<fmt::format("{:%c}", *std::localtime(&end_time_t))<<endl;
        cout<<"  total (elapsed) time: "<<duration_string( end_time - start_work_time )<<endl;
        cout<<"  total (CPU) time: "<<duration_string( end_cpu_time - start_work_cpu_time )<<endl;
    }

    if (substitution::total_calc_root_prob > 1 and log_verbose >= 1)
    {
        cout<<endl;
        cout<<"total likelihood evals = "<<substitution::total_likelihood<<endl;
        cout<<"total calc_root_prob evals = "<<substitution::total_calc_root_prob<<endl;
        cout<<"total branches peeled = "<<substitution::total_peel_leaf_branches+substitution::total_peel_internal_branches<<endl;
        cout<<"average root clv length = "<<substitution::total_root_clv_length/substitution::total_calc_root_prob<<endl;
        cout<<endl;
    }
}

std::shared_ptr<module_loader> setup_module_loader(variables_map& args)
{
    auto L = std::make_shared<module_loader>(get_cache_path(), get_package_paths(args));

    // 4. Write out paths to C1.err
    if (log_verbose >= 1)
    {
        std::cout<<"\nPackage path = \n";
        for(const auto& path: L->plugins_path)
            cout<<"  "<<path<<"\n";
        cout<<std::endl;
    }

    myexception e;
    e<<"Can't find associated files -- bali-phy is not correctly installed!  \n";

    // 5a. Check for Prelude.so
    bool found_prelude = true;
    try
    {
        L->find_plugin("Prelude");
        e<<"   + Found "<<fs::path("Prelude"+plugin_extension)<<".\n";
    }
    catch (...)
    {
        // NB - We should specify what the lookup rules are.
        // If all the user-specified paths are empty, we should probably say so.
        // We shouldn't expect to find the Prelude plugin in the user lib path.
        e<<"   + Can't find "<<fs::path("Prelude"+plugin_extension)<<".\n";
        found_prelude = false;
    }

    // 5b. Check for Prelude.hs
    try
    {
        L->find_module("Prelude");
        e<<"   + Found "<<fs::path("modules") / "Prelude.hs"<<".\n";
    }
    catch (...)
    {
        e<<"   + Can't find "<<fs::path("modules") / "Prelude.hs"<<".\n";
        found_prelude = false;
    }

    // 5c. Check for empty paths
    e<<" * These are usually in the directory "<< fs::path("..") / "lib" / "bali-phy" <<" relative to the executable.\n";
    if (L->plugins_path.empty())
    {
        e<<" * But that directory does not exist.\n";
        e<<" * Did you move the executable?";
    }

    if (not found_prelude)
    {
        throw e;
    }

    L->pre_inline_unconditionally = args["pre-inline"].as<bool>();
    L->post_inline_unconditionally = args["post-inline"].as<bool>();
    L->let_float_from_case = args["let-float-from-case"].as<bool>();
    L->let_float_from_apply = args["let-float-from-apply"].as<bool>();
    L->let_float_from_let = args["let-float-from-let"].as<bool>();
    L->case_of_constant = args["case-of-constant"].as<bool>();
    L->case_of_variable = args["case-of-variable"].as<bool>();
    L->case_of_case = args["case-of-case"].as<bool>();
    L->inline_threshhold = args["inline-threshold"].as<int>();
    L->keenness = args["keenness"].as<double>();
    L->beta_reduction = args["beta-reduction"].as<bool>();
    L->max_iterations = args["simplifier-max-iterations"].as<int>();

    L->fully_lazy = args["fully-lazy"].as<bool>();
    L->dump_parsed = args.count("dump-parsed");
    L->dump_renamed = args.count("dump-rn");
    L->dump_typechecked = args.count("dump-tc");
    L->dump_desugared = args.count("dump-ds");
    L->dump_optimized = args.count("dump-opt");

    if (args.count("recompile"))
    {
        auto recompile_string = args.at("recompile").as<string>();
        if (recompile_string == "none" or recompile_string == "no")
            ;
        else if (recompile_string.empty())
            L->recompile_all = true;
        else
        {
            for(auto& modid: split(recompile_string, ','))
                L->recompile_modules.insert(modid);
        }
    }
    else if (args.count("test-module"))
    {
        L->recompile_modules.insert(args.at("test-module").as<string>());
    }

    return L;
}

std::string generate_print_program(const model_t& print, const expression_ref& a)
{
    std::ostringstream program_file;

    program_file<<"-- Use the program `ormolu` (or `brittany` or `hindent`) to indent this file for readability\n";
    set<string> imports;
    imports.insert("Probability.Random");     // for runRandomLazy
    imports.insert("Bio.Alphabet");               // for dna
    add(imports, print.imports);
    for(auto& mod: imports)
        program_file<<"import "<<mod<<"\n";

    program_file<<"\n";
    program_file<<"print_model = "<<print.code.print()<<"\n";
    program_file<<"\n";
    program_file<<"main = do\n";

    expression_ref E = var("print_model");
    for(auto& state_name: print.code.used_states)
    {
        if (state_name == "alphabet")
            E = {E, a};
        else if (state_name == "branch_categories")
        {
            throw myexception()<<"Can't handle state 'branch_categories' in --print expressions right now.";
//            smodel = {smodel, branch_categories};
        }
        else if (state_name == "tree")
        {
            throw myexception()<<"Can't handle state 'tree' in --print expressions right now.";
//            smodel = {smodel, branch_categories};
        }
        else
            throw myexception()<<"Don't know how to supply variable for state '"<<state_name<<"'";
    }

    if (print.code.is_action())
        program_file<<"  result <- runRandomLazy ("<<E<<")\n";
    else
        program_file<<"  let result = "<<E<<"\n";

    if (print.code.has_loggers())
        program_file<<"  putStrLn $ show $ fst result\n";
    else
        program_file<<"  putStrLn $ show $ result\n";

    return program_file.str();
}

expression_ref get_alphabet_expression_from_args(const variables_map& args)
{
    if (not args.count("alphabet") or args.at("alphabet").as<vector<string>>().empty())
	return {var("error"),String("No alphabet!")};

    auto anames = args.at("alphabet").as<vector<string>>();
    if (anames.size() > 1)
	throw myexception()<<"Only a single alphabet can be used with --print!";

    return get_alphabet_expression( *get_alphabet(anames[0]) );
}

std::unique_ptr<Program> print_program(variables_map& args, const shared_ptr<module_loader>& L)
{
    const string mstring = args["print"].as<string>();
    Rules R(get_package_paths(args));
    auto TC = makeTypechecker(R, {}, {{"alphabet",{"alphabet","b"}}});
    model_t print = compile_model(R, TC, CodeGenState(R), "a", mstring, "print expression", {}, {{"alphabet",{"alphabet","b"}}});

    expression_ref a = get_alphabet_expression_from_args(args);
    {
        checked_ofstream program_file("Print.Main.hs");
        program_file<<generate_print_program(print, a);
    }
    return load_program_from_file(L, "Print.Main.hs");
}

std::pair<fs::path, vector<string>> extract_prog_args(variables_map& args, int argc, char* argv[], const string& cmd)
{
    auto args_v = args[cmd].as<vector<string>>();

    if (args_v.empty())
        throw myexception()<<"--"<<cmd<<" requires at least one argument";

    fs::path name = args_v[0];
    if (name.extension() != ".hs")
        name += ".hs";

    args_v.erase(args_v.begin());

    for(auto& arg: trailing_args(argc, argv, trailing_args_separator))
        args_v.push_back(arg);

    return {name, args_v};
}

std::unique_ptr<Program> generate_program(int argc, char* argv[], variables_map& args, const shared_ptr<module_loader>& L,
                                          int proc_id, const fs::path& output_dir, json::object info)
{
    std::unique_ptr<Program> P;

    if (args.count("run"))
    {
        auto [main_filename, args_v] = extract_prog_args(args, argc, argv, "run");
        L->args = args_v;
        P = load_program_from_file(L, main_filename);
    }
    else if (args.count("print"))
    {
        P = print_program(args, L);
    }
    else if (args.count("align"))
    {
        // Change this into a pointer.
        Rules R(get_package_paths(args));
        auto [prog, j] = create_A_and_T_model(R, args, L, proc_id, output_dir);
        update(info, j);
        P = std::move(prog);
    }
    else if (args.count("model"))
    {
        auto [model_filename, args_v] = extract_prog_args(args, argc, argv, "model");
        L->args = args_v;
        P = gen_model_program(args, L, output_dir, model_filename);
    }
    else
    {
	std::cerr<<"generate_program: no 'run', 'print', 'align', or 'model'";
        std::abort();
    }

    //------ Write run info to C1.run.json ------//
    if ((args.count("align") or args.count("model")) and not args.count("test"))
    {
        ofstream run_info( output_dir / "C1.run.json" );
	run_info<<json::serialize_options({.allow_infinity_and_nan=true});
        run_info<<info<<std::endl;
        run_info.close();
        cout<<"Run info written to "<< output_dir / "C1.run.json" <<endl;

        long int max_iterations = 200000;
        if (args.count("iterations"))
            max_iterations = args["iterations"].as<long int>();

        if (args.count("align"))
            write_initial_alignments(args, proc_id, output_dir);

        //------ Report log files -------//
        cout<<"\nBeginning MCMC computations."<<endl;
        auto log_formats = get_log_formats(args, args.count("align"));
        if (log_formats.count("json"))
            cout<<"   - Sampled "<<bold_blue("numerical parameters")<<" logged to "<< output_dir / "C1.log.json" <<" as JSON\n";
        if (log_formats.count("tsv"))
            cout<<"   - Sampled "<<bold_blue("numerical parameters")<<" logged to "<< output_dir / "C1.log" << " as TSV\n";
        if (args.count("align"))
        {
            cout<<"   - Sampled "<<bold_green("trees")<<" logged to "<< output_dir / "C1.trees" <<endl;
            cout<<"   - Sampled "<<bold_red("alignments")<<" logged to "<< output_dir / "C1.P<partition>.fastas" <<endl;
        }

        //------ Clarify lack of auto-stopping -----/
        cout<<"\nBAli-Phy does NOT detect how many iterations is sufficient:\n   You need to monitor convergence and kill it when done."<<endl;
        if (not args.count("iterations"))
            cout<<"   Maximum number of iterations not specified: limiting to "<<max_iterations<<"."<<endl;
        else
            cout<<"   Maximum number of iterations set to "<<max_iterations<<"."<<endl;

        cout<<"\n";
        if (log_formats.count("tsv"))
            cout<<"You can examine 'C1.log' using BAli-Phy tool statreport (command-line) or the BEAST program Tracer (graphical).\n";
        cout<<"See the manual at http://www.bali-phy.org/README.xhtml for further information.\n";
        cout.flush();
    }

    return P;
}


int main(int argc,char* argv[])
{ 
    int n_procs = 1;
    int proc_id = 0;

#ifdef HAVE_MPI
    mpi::environment env(argc, argv);
    mpi::communicator world;

    proc_id = world.rank();
    n_procs = world.size();
#endif

    std::ios::sync_with_stdio(false);

    int retval=0;

    try {
// This should probably be a run-time switch.
#if defined(HAVE_FEENABLEEXCEPT) && defined(DEBUG_FPE)
        feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif
#if defined(HAVE_CLEAREXCEPT) && !defined(DEBUG_FPE)
        feclearexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif
        fp_scale::initialize();

        //---------- Parse command line  ---------//
        variables_map args = parse_cmd_line(argc,argv);

        //------ Increase precision for (cout,cerr) if we are testing ------//
        if (args.count("test"))
        {
            cerr.precision(15);
            cout.precision(15);
        }

        //------------- Setup module loader -------------//
        auto L = setup_module_loader(args);
        L->args = trailing_args(argc, argv, trailing_args_separator);
        L->optimize = args["optimize"].as<bool>();

        //---------- Initialize random seed -----------//
        unsigned long seed = init_rng_and_get_seed(args);

        if (log_verbose >= 1) cout<<"random seed = "<<seed<<endl<<endl;

        //---------- test optimizer ----------------
        if (args.count("test-module"))
        {
            string module_name = args["test-module"].as<string>();

            std::shared_ptr<Module> M;
            if (is_haskell_module_name(module_name))
                M = L->load_module(module_name);
            else
                M = L->load_module_from_file(module_name);

            Program P(L,{M});
            auto M2 = P.get_module(M->name);
            for(auto& [name,symbol]: M2->exported_symbols())
            {
                TidyState tidy_state;
                std::cerr<<name<<" :: "<<tidy_state.print(symbol->type)<<"\n\n";
            }

            exit(0);
        }
        else if (args.count("type"))
        {
            auto term = args.at("type").as<string>();

            auto module_name = get_module_name(term);
            if (module_name.empty()) module_name = "Prelude";
            auto M = L->load_module(module_name);

            std::vector<std::shared_ptr<Module>> modules;
            modules.push_back(M);
            auto P = std::make_unique<Program>(L, modules);
            auto compiled_mod = P->get_module(module_name);

            auto symbol = compiled_mod->lookup_symbol(get_unqualified_name(term));
            auto type = symbol->type;
            std::cout<<"name:  "<<symbol->name<<"\n";

            TidyState tidy_state;
            tidy_state.ignore_top_foralls = true;
            std::cout<<"type:  "<<tidy_state.print(type)<<"\n";
            exit(0);
        }


        //----------- Create output dir --------------//
        fs::path output_dir;

        if ((args.count("align") or args.count("model")) and not args.count("test"))
        {
#ifdef HAVE_MPI
            // FIXME: Can we just use `broadcast(world, output_dir, 0)`?
            //        This might require a serializer for fs::path.
            if (not proc_id) {
                output_dir = init_dir(args);

                for(int dest=1;dest<n_procs;dest++) 
                    world.send(dest, 0, output_dir.string());
            }
            else
            {
                string dir_name;
                world.recv(0, 0, dir_name);
                output_dir = dir_name;
            }

            // cerr<<"Proc "<<proc_id<<": dirname = "<<dir_name<<endl;
#else
            output_dir = init_dir(args);
#endif
        }

        //---------- Create model object -----------//
        json::object info;
        run_info(info, proc_id, argc, argv);
        info["seed"] = seed;
        info["subdirectory"] = fs::weakly_canonical(output_dir).make_preferred().string();

        auto P = generate_program(argc, argv, args, L, proc_id, output_dir, info);

        L.reset();

        raise_cpu_limit(cout);

        block_signals();

        start_work_time = chrono::system_clock::now();
        start_work_cpu_time = total_cpu_time();

        // Run the program P
        execute_program( std::move(P) );
    }
    catch (std::bad_alloc&) 
    {
        // 1. Now, write message to either (screen+cache) or (screen+file), and flush.
        cerr<<"Doh!  Some kind of memory problem?\n"<<endl;
        // 2. Write memory report to either (screen) or (screen+cache) or (screen+file)
        report_mem();
        retval=2;
    }
    catch (std::exception& e) 
    {
        // 1. Now, write message to either (screen+cache) or (screen+file), and flush.
        if (n_procs > 1)
            cerr<<"bali-phy: Error["<<proc_id<<"]! "<<e.what()<<endl;
        else
            cerr<<"bali-phy: Error! "<<e.what()<<endl;

        retval=1;

/*        
        if (log_verbose > 0 and M)
        {
            try
            {
                M->show_graph_for_root_token();
            }
            catch(...)
            {
                cerr<<"\n\nError thrown while printing graph after catching exception!\n\n";
            }
        }
*/
    }

    show_ending_messages();

    cout.flush();
    cerr.flush();
    return retval;
}

