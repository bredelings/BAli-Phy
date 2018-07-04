libsums = static_library('libsums','computation/machine/sums.cc', dependencies: boost, cpp_args: '-fno-fast-math', install: false)

git_version_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
  replace_string: '@git_message@',
  fallback: '',
)

extra_link_args = []
if get_option('static-cpp-libs')
  extra_link_args += ['-static-libgcc','-static-libstdc++']
endif

# tools/findroot.cc -> tools/optimize.cc
libbaliphy_sources = ['io.cc','util.cc','tree/sequencetree.cc','tree/tree.cc','sequence/alphabet.cc','sequence/sequence.cc','tree/tree-util.cc','tools/read-trees.cc','sequence/sequence-format.cc','alignment/alignment-util.cc','rng.cc','alignment/load.cc','alignment/alignment.cc','tools/statistics.cc','tools/partition.cc','tools/tree-dist.cc','alignment/alignment-random.cc','setup.cc','tree/randomtree.cc','util-random.cc','tools/parsimony.cc','alignment/index-matrix.cc','tools/mctree.cc','tools/stats-table.cc','tools/findroot.cc','tools/optimize.cc','tools/distance-report.cc','n_indels.cc','tools/inverse.cc','tools/joint-A-T.cc','tools/distance-methods.cc','tools/consensus-tree.cc']

libbaliphy = static_library('bali-phy', libbaliphy_sources, 
			    dependencies: [boost, eigen],
			    link_args: extra_link_args)

baliphy_sources = ['parser/parse.cc','dp/dp_hmm.cc','parser/desugar.cc',
   'substitution/substitution.cc', 'util/ptree.cc',
   'mcmc/moves.cc', 'math/exponential.cc','math/eigenvalue.cc',
   'models/parameters.cc','prior.cc','mcmc/mcmc.cc', 'probability/choose.cc',
   'mcmc/sample-branch-lengths.cc',
   'bali-phy.cc',
   'models/TreeInterface.cc', 'dp/hmm.cc','dp/dp-engine.cc','dp/dp-array.cc',
   'dp/dp-matrix.cc','dp/3way.cc','dp/2way.cc','mcmc/sample-alignment.cc',
   'mcmc/sample-node.cc','imodel/imodel.cc','dp/5way.cc','mcmc/sample-topology-NNI.cc',
   'mcmc/sample-two-nodes.cc', 'dp/dp-cube.cc', 'mcmc/sample-cube.cc',
   'models/setup.cc', 'startup/help.cc',
   'mcmc/sample-topology-SPR.cc', 'dp/alignment-sums.cc',
   'probability/probability.cc','models/model.cc', 'alignment/alignment-constraint.cc',
   'monitor.cc', 'myexception.cc','math/pow2.cc',
   'substitution/parsimony.cc', 'mcmc/proposals.cc',
   'n_indels2.cc','alignment/alignment-util2.cc',
   'tools/parsimony2.cc','version.cc','mcmc/slice-sampling.cc','timer_stack.cc',
   'mcmc/setup.cc','mcmc/logger.cc','mcmc/AIS.cc','computation/operator.cc',
   'computation/expression/expression.cc','computation/expression/constructor.cc',
   'computation/expression/expression_ref.cc','computation/expression/AST_node.cc',
   'computation/expression/apply.cc','computation/expression/substitute.cc',
   'computation/expression/indexify.cc','computation/expression/let.cc',
   'computation/expression/case.cc','computation/expression/trim.cc',
   'computation/expression/tuple.cc','computation/expression/list.cc',
   'computation/expression/var.cc','computation/expression/lambda.cc',
   'computation/computation.cc','computation/machine/tokens.cc',
   'computation/preprocess.cc', 'computation/machine/graph_register.cc',
   'computation/machine/show_graph.cc','computation/module.cc',
   'computation/machine/evaluate.cc',
   'computation/machine/gc.cc','computation/machine/reroot.cc',
   'computation/operations.cc','computation/loader.cc','computation/context.cc',
   'computation/closure.cc', 'computation/optimization/let-float.cc',
   'computation/program.cc','mcmc/sample-tri.cc','startup/A-T-model.cc',
   'startup/files.cc', 'startup/loggers.cc','startup/system.cc','startup/cmd_line.cc',
   'models/rules.cc','models/parse.cc', 'models/translate.cc','models/unification.cc',
   'computation/optimization/simplifier.cc',
   'startup/paths.cc', 'computation/optimization/occurrence.cc',
   'computation/optimization/inliner.cc',
   git_version_h]


libdl = []
extra_includes = []
if host_machine.system() == 'windows'
  # We should have a system of 'cygwin' for cygwin.
  baliphy_sources += [ 'dlfcn-win32/dlfcn.c' ]
  extra_includes = include_directories('dlfcn-win32/')
else
  libdl = cpp.find_library('dl')
endif

# maybe we don't need this if we are doing static linking
extra_rpath = ''
if host_machine.system() == 'linux'
  extra_rpath = '$ORIGIN/../lib/bali-phy/'
endif

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: [root_inc,  extra_includes],
		     dependencies: [boost, eigen, json, libdl],
		     link_args: extra_link_args,
		     link_with: [libbaliphy, libsums],
		     install_rpath: extra_rpath,
		     install: true,
		     export_dynamic: true)

subdir('builtins')

test('bali-phy version', baliphy, args:['--version'])
test('bali-phy help', baliphy, args:['--help'])
test('bali-phy 5d test', baliphy, args:[small_fasta,'--test', packagepath])
# When running on very slow autobuilders these tests could take a long time.
test('bali-phy 5d +A 50', baliphy, args:[small_fasta,'--iter=50', packagepath], timeout: 180)
test('bali-phy 5d -A 200', baliphy, args:[small_fasta,'--iter=200', packagepath, '-Inone'], timeout:120)

#--------- Build rules for tools that are always installed ------------#

simple_tools = ['model_P','statreport','stats-select','alignment-gild','alignment-consensus', 'alignment-max','alignment-chop-internal',
		'alignment-indices','alignment-info','alignment-cat','alignment-translate','alignment-find','trees-consensus',
		'tree-mean-lengths','mctree-mean-lengths','trees-to-SRQ','pickout','subsample','cut-range','trees-distances', 'alignment-thin',
		'alignments-diff','tree-tool']

foreach tool: simple_tools
  tool_exe = executable(tool,
			['tools/' + tool + '.cc'],
			dependencies:boost,
			link_with: libbaliphy,
			install_rpath: extra_rpath,
			install: true)
  test(tool + ' --help', tool_exe, args:['--help'])
endforeach

executable('alignment-draw',  ['tools/alignment-draw.cc', 'tools/colors.cc'] , dependencies:boost, link_with: libbaliphy, install: true)

executable('trees-bootstrap', ['tools/trees-bootstrap.cc', 'tools/bootstrap.cc'], dependencies:boost, link_with: libbaliphy, install: true)

executable('draw-tree', ['tools/draw-tree.cc'], dependencies: [boost, cairo], include_directories: root_inc, link_with: libbaliphy, install:true)

all_progs = ['bali-phy'] + simple_tools + ['alignment-draw','trees-bootstrap','draw-tree']

#--------- Build rules for tools that are SOMETIMES installed ------------#

if get_option('extra-tools')
  extra_tools = ['stats-merge', 'stats-cat', 'alignment-median', 'alignment-identity', 'alignment-compare', 'joint-indels', 'joint-parsimony', 'alignment-smc',
		 'alignment-convert', 'alignment-find-conserved','partitions-supported','draw-graph','trees-pair-distances','tree-partitions','tree-reroot',
		 'path-graph']
  foreach tool : extra_tools
    executable( tool,
		['tools/' + tool + '.cc'],
		dependencies: boost,
		link_with:libbaliphy,
		install_rpath: extra_rpath,
		install:true)
  endforeach
  all_progs = all_progs + extra_tools
endif

