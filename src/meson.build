git_version_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
  replace_string: '@git_message@',
  fallback: '',
)

subdir('util')

subdir('mcon')

libcommon_sources = [
  'tree/newick-tokenizer.cc',
  'tree/sequencetree.cc',
  'tree/tree.cc',
  'tree/tree-util.cc',
  'tree/randomtree.cc',
  'tree-align/n_indels.cc',

  'sequence/alphabet.cc',
  'sequence/doublets.cc',
  'sequence/RNAEdits.cc',
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
                           include_directories: [libutil_inc, libmcon_inc],
                           dependencies: [boost, eigen, librange],
                           link_with: libmcon)

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
  'dp/4way.cc',
  'dp/5way.cc',
  'dp/alignment-sums.cc',

  'mcmc/result.cc',
  'mcmc/slice-sampling.cc',
  'mcmc/moves.cc',
  'mcmc/sample-branch-lengths.cc',
  'mcmc/sample-alignment.cc',
  'mcmc/sample-topology-SPR.cc',
  'mcmc/sample-topology-NNI.cc',
  'mcmc/sample-A4.cc',
  'mcmc/sample-A5.cc',
  'mcmc/sample-A5-2D.cc',
  'mcmc/sample-node.cc',
  'mcmc/sample-cube.cc',
  'mcmc/sample-tri.cc',

  'substitution/ops.cc',
  'substitution/cache.cc',
  'substitution/likelihood.cc',
  'substitution/likelihoodSEV.cc',
  'substitution/parsimony.cc',

  'math/exponential.cc',
  'math/pow2.cc',

  'models/TreeInterface.cc',
  'models/parameters.cc',
  'models/A-T-prog.cc',

  'models/rules.cc',
  'models/parse.cc',
  'models/driver.cc',
  'models/parser.cc',
  'models/lexer.cc',
  'models/compile.cc',
  'models/typecheck.cc',
  'models/unification.cc',
  'models/code-generation.cc',
  'models/logger.cc',

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
			    dependencies: [boost, eigen, libdl, math_dep, librange, immer, cereal])
			    
subdir('bali-phy')

subdir('builtins')

test('bali-phy version', baliphy, args:['--version'])
test('bali-phy help', baliphy, args:['--help'])
# When running on very slow autobuilders these tests could take a long time.
test('bali-phy 5d test', baliphy, args:[small_fasta,'--test', packagepath], timeout: 300)
test('bali-phy 5d +A 50', baliphy, args:[small_fasta,'--iter=50', '-V', packagepath], timeout: 300)
test('bali-phy 48 +A 3', baliphy, args:[big_fasta,'--iter=3', '-V', packagepath], timeout: 600)
test('bali-phy 5d -A 200', baliphy, args:[small_fasta,'--iter=200', '-V', packagepath, '-Inone'], timeout:300)

all_progs = ['bali-phy']

#--------- Build rules for tools that are always installed ------------#

tools = ['model_P','statreport','stats-select','alignment-gild','alignment-consensus', 'alignment-max','alignment-chop-internal',
	 'alignment-indices','alignment-info','alignment-cat','alignment-translate','alignment-find','trees-consensus',
	 'tree-mean-lengths','mctree-mean-lengths','trees-to-SRQ','pickout','cut-range','trees-distances', 'alignment-thin',
	 'alignments-diff','tree-tool','alignment-distances','alignment-draw', 'trees-bootstrap', 'draw-tree', 'extract-ancestors',
         'summarize-ancestors','mcon-tool']

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

joint_indels_exe = disabler()

foreach tool: tools + prefixed_tools
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

  tool_exe = executable(name,
			sources,
			include_directories: [root_inc, libutil_inc, libmcon_inc],
			dependencies: deps,
			link_with: [libutil,libcommon,libmcon],
			install_rpath: extra_rpath,
			install: true)

  test(tool + ' --help', tool_exe, args:['--help'])

  if tool == 'joint-indels'
    joint_indels_exe = tool_exe
  endif
endforeach

# joint-indels unit tests (only if extra-tools enabled)
if not is_disabler(joint_indels_exe)
  joint_indels_test_script = files('../tests/tools/joint-indels/run-tests.sh')
  test('joint-indels --details',
       find_program('bash'),
       args: [joint_indels_test_script, joint_indels_exe],
       workdir: meson.project_source_root())
endif
