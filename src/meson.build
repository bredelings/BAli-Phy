git_version_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
  replace_string: '@git_message@',
  fallback: '',
)

libutil_sources = [
  'util/string/pred.cc',
  'util/string/sanitize.cc',
  'util/log-level.cc',
  'util/io.cc',
  'util/range.cc',
  'util/demangle.cc',
  'util/mapping.cc',
  'util/string/strip.cc',
  'util/string/split.cc',
  'util/cmdline.cc',
  'util/rng.cc',
  'util/permute.cc',
  'util/object.cc',
  'util/file-paths.cc',
  'util/ptree.cc',
  'util/text.cc',
  'util/myexception.cc',
  'util/time.cc']

libutil = static_library('util', libutil_sources, 
			 dependencies: [boost, eigen, json, librange])


libcommon_sources = [
  'tree/newick-tokenizer.cc',
  'tree/sequencetree.cc',
  'tree/tree.cc',
  'tree/tree-util.cc',
  'tree/randomtree.cc',
  'tree-align/n_indels.cc',

  'sequence/alphabet.cc',
  'sequence/sequence.cc',
  'sequence/sequence-format.cc',

  'tools/read-trees.cc',

  'alignment/alignment-util.cc',
  'alignment/load.cc',
  'alignment/alignment.cc',
  'alignment/alignment-random.cc',
  'alignment/index-matrix.cc',

  'tree-align/link.cc',

  'tools/statistics.cc',
  'tools/partition.cc',
  'tools/tree-dist.cc',
  'tools/parsimony.cc',
  'tools/mctree.cc',
  'tools/stats-table.cc',
  'tools/findroot.cc',
  'tools/optimize.cc',
  'tools/distance-report.cc',
  'tools/inverse.cc',
  'tools/joint-A-T.cc',
  'tools/distance-methods.cc',
  'tools/consensus-tree.cc']



libcommon = static_library('common', libcommon_sources, 
			    dependencies: [boost, eigen, librange])

subdir('computation')

baliphy_sources = [
  'dp/dp_hmm.cc',
  'substitution/substitution.cc', 'bali-phy/link-partitions.cc',
  'mcmc/moves.cc', 'math/exponential.cc','math/eigenvalue.cc',
  'models/parameters.cc','mcmc/mcmc.cc', 'probability/choose.cc',
  'mcmc/sample-branch-lengths.cc', 'bali-phy/bali-phy.cc',
  'models/TreeInterface.cc',
  'dp/hmm.cc','dp/dp-engine.cc','dp/dp-array.cc',
  'dp/dp-matrix.cc','dp/3way.cc','dp/2way.cc','mcmc/sample-alignment.cc',
  'mcmc/sample-node.cc','imodel/imodel.cc','dp/5way.cc','mcmc/sample-topology-NNI.cc',
  'mcmc/sample-two-nodes.cc', 'dp/dp-cube.cc', 'mcmc/sample-cube.cc',
  'models/setup.cc', 'bali-phy/help.cc',
  'mcmc/sample-topology-SPR.cc', 'dp/alignment-sums.cc',
  'probability/probability.cc','models/model.cc', 'alignment/alignment-constraint.cc',
  'math/pow2.cc',
  'substitution/parsimony.cc', 'mcmc/proposals.cc',
  'tree-align/n_indels2.cc','alignment/alignment-util2.cc',
  'tools/parsimony2.cc','version.cc','mcmc/slice-sampling.cc',
  'mcmc/setup.cc','mcmc/logger.cc','mcmc/AIS.cc',
  'mcmc/sample-tri.cc','bali-phy/A-T-model.cc',
  'bali-phy/files.cc', 'bali-phy/loggers.cc','bali-phy/system.cc','bali-phy/cmd_line.cc',
  'models/rules.cc','models/parse.cc', 'models/translate.cc','models/unification.cc',
  'bali-phy/paths.cc',
  git_version_h]


# maybe we don't need this if we are doing static linking
extra_rpath = ''
if host_machine.system() == 'linux'
  extra_rpath = '$ORIGIN/../lib/bali-phy/'
endif

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: root_inc,
		     dependencies: [boost, eigen, json, libdl, math_dep, librange],
		     link_with: [libutil, libcommon, libcomputation],
		     install_rpath: extra_rpath,
		     install: true,
		     export_dynamic: true)

subdir('computation/builtins')

test('bali-phy version', baliphy, args:['--version'])
test('bali-phy help', baliphy, args:['--help'])
test('bali-phy 5d test', baliphy, args:[small_fasta,'--test', packagepath])
# When running on very slow autobuilders these tests could take a long time.
test('bali-phy 5d +A 50', baliphy, args:[small_fasta,'--iter=50', packagepath], timeout: 180)
test('bali-phy 48 +A 3', baliphy, args:[big_fasta,'--iter=3', packagepath], timeout: 360)
test('bali-phy 5d -A 200', baliphy, args:[small_fasta,'--iter=200', packagepath, '-Inone'], timeout:120)

all_progs = ['bali-phy']

#--------- Build rules for tools that are always installed ------------#

tools = ['model_P','statreport','stats-select','alignment-gild','alignment-consensus', 'alignment-max','alignment-chop-internal',
		'alignment-indices','alignment-info','alignment-cat','alignment-translate','alignment-find','trees-consensus',
		'tree-mean-lengths','mctree-mean-lengths','trees-to-SRQ','pickout','cut-range','trees-distances', 'alignment-thin',
		'alignments-diff','tree-tool','alignment-distances','alignment-draw', 'trees-bootstrap', 'draw-tree']

extra_tools = ['stats-merge', 'stats-cat', 'alignment-identity', 'alignment-compare', 'joint-indels', 'joint-parsimony', 'alignment-smc',
		 'alignment-convert', 'alignment-find-conserved', 'partitions-supported', 'draw-graph', 'trees-pair-distances', 'tree-partitions', 'tree-reroot',
		 'path-graph']

if get_option('extra-tools')
  tools += extra_tools
endif

tools_prefix = ['subsample']

prefix = 'bali-'

prefixed_tools = ['subsample']

extra_sources = {'alignment-draw': ['tools/colors.cc'], 'trees-bootstrap': ['tools/bootstrap.cc']}

extra_deps = {'draw-tree': [cairo]}

foreach tool: tools
  sources = ['tools/' + tool + '.cc']
  if tool in extra_sources
    sources += extra_sources[tool]
  endif

  deps = [boost, librange, math_dep]
  if tool in extra_deps
    deps += extra_deps[tool]
  endif

  name = tool
  if tool in prefixed_tools
    name = prefix + name
  endif

  all_progs += name

  tool_exe = executable(name, sources, include_directories: root_inc, dependencies: deps, link_with: [libutil,libcommon], install_rpath: extra_rpath, install: true)

  test(tool + ' --help', tool_exe, args:['--help'])
endforeach
