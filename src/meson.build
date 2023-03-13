git_version_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
  replace_string: '@git_message@',
  fallback: '',
)

subdir('util')

libcommon_sources = [
  'tree/newick-tokenizer.cc',
  'tree/sequencetree.cc',
  'tree/tree.cc',
  'tree/tree-util.cc',
  'tree/randomtree.cc',
  'tree-align/n_indels.cc',

  'sequence/alphabet.cc',
  'sequence/doublets.cc',
  'sequence/triplets.cc',
  'sequence/genetic_code.cc',
  'sequence/codons.cc',
  'sequence/sequence.cc',
  'sequence/sequence-format.cc',

  'tools/read-trees.cc',

  'alignment/alignment-util.cc',
  'alignment/load.cc',
  'alignment/alignment.cc',
  'alignment/alignment-random.cc',
  'alignment/index-matrix.cc',

  'tree-align/link.cc',

  'models/path.cc',

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



libcommon = static_library('common',
			   libcommon_sources,
			   include_directories: libutil_inc,
			   dependencies: [boost, eigen, librange, json])

subdir('computation')

libbaliphy_sources = [
  'dp/dp_hmm.cc',
  'dp/hmm.cc',
  'dp/dp-engine.cc',
  'dp/dp-array.cc',
  'dp/dp-matrix.cc',
  'dp/dp-cube.cc',
  'dp/2way.cc',
  'dp/3way.cc',
  'dp/5way.cc',
  'dp/alignment-sums.cc',

  'mcmc/mcmc.cc',
  'mcmc/proposals.cc',
  'mcmc/slice-sampling.cc',
  'mcmc/AIS.cc',
  'mcmc/logger.cc',
  'mcmc/moves.cc',
  'mcmc/sample-branch-lengths.cc',
  'mcmc/sample-alignment.cc',
  'mcmc/sample-topology-SPR.cc',
  'mcmc/sample-topology-NNI.cc',
  'mcmc/sample-two-nodes.cc',
  'mcmc/sample-node.cc',
  'mcmc/sample-cube.cc',
  'mcmc/sample-tri.cc',
  'mcmc/setup.cc',

  'substitution/substitution.cc',
  'substitution/parsimony.cc',

  'math/exponential.cc',
  'math/eigenvalue.cc',
  'math/pow2.cc',

  'models/parameters.cc',
  'models/site-compression.cc',
  'models/TreeInterface.cc',
  'models/setup.cc',
  'models/rules.cc',
  'models/parse.cc',

  'models/driver.cc',
  'models/parser.cc',
  'models/lexer.cc',

  'models/typecheck.cc',
  'models/unification.cc',
  'models/model.cc',

  'tree-align/link2.cc',

  'probability/choose.cc',
  'probability/probability.cc',

  'imodel/imodel.cc',

  'alignment/alignment-constraint.cc',
  'alignment/alignment-util2.cc',

  'tree-align/n_indels2.cc',
  'tools/parsimony2.cc',
  'version.cc',
  git_version_h]

libbaliphy = static_library('baliphy', libbaliphy_sources, 
			    include_directories: [root_inc, libutil_inc],
			    dependencies: [boost, eigen, json, libdl, math_dep, librange, immer])
			    
subdir('bali-phy')

subdir('builtins')

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
	 'alignments-diff','tree-tool','alignment-distances','alignment-draw', 'trees-bootstrap', 'draw-tree', 'extract-ancestors',
         'summarize-ancestors']

extra_tools = ['stats-merge', 'stats-cat', 'alignment-identity', 'alignment-compare', 'joint-indels', 'joint-parsimony', 'alignment-smc',
		 'alignment-convert', 'alignment-find-conserved', 'partitions-supported', 'draw-graph', 'trees-pair-distances', 'tree-partitions', 'tree-reroot',
		 'path-graph']

if get_option('extra-tools')
  tools += extra_tools
endif

prefix = 'bali-'

prefixed_tools = ['subsample']

extra_sources = {'alignment-draw': ['tools/colors.cc'], 'trees-bootstrap': ['tools/bootstrap.cc']}

extra_deps = {'draw-tree': [cairo]}

foreach tool: tools + prefixed_tools
  sources = ['tools/' + tool + '.cc']
  if tool in extra_sources
    sources += extra_sources[tool]
  endif

  deps = [boost, librange, math_dep, json]
  if tool in extra_deps
    deps += extra_deps[tool]
  endif

  name = tool
  if tool in prefixed_tools
    name = prefix + name
  endif

  all_progs += name

  tool_exe = executable(name,
			sources,
			include_directories: [root_inc, libutil_inc],
			dependencies: deps,
			link_with: [libutil,libcommon],
			install_rpath: extra_rpath,
			install: true)

  test(tool + ' --help', tool_exe, args:['--help'])
endforeach
