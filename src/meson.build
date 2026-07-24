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
  'models/model-expr.cc',
  'models/model-type.cc',

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

builtins_path = builtins
subdir('builtins')

test('bali-phy version', baliphy, args:['--version'], is_parallel: false)
test('bali-phy help', baliphy, args:['--help'], is_parallel: false)

module_cache_wrapper_args = []
if host_machine.system() == 'windows' and meson.is_cross_build()
  module_cache_wrapper_args = ['--wrapper'] + windows_test_wrapper
endif
test('compiled module cache',
     python,
     args: [files('../tests/scripts/test_module_cache.py')] +
           module_cache_wrapper_args +
           ['--', baliphy.full_path(),
            meson.project_source_root()/'tests/haskell/ModuleCache',
            packagepath],
     timeout: 120)

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

alignment_draw_assets = custom_target(
  'alignment-draw-assets',
  input: ['tools/alignment-draw.js', 'tools/alignment-draw.css'],
  output: 'alignment-draw-assets.hh',
  command: [python, files('../scripts/embed-text.py'), '@OUTPUT@',
            'alignment_draw_javascript', '@INPUT0@',
            'alignment_draw_stylesheet', '@INPUT1@'])

extra_sources = {
  'alignment-draw': ['tools/colors.cc', alignment_draw_assets],
  'trees-bootstrap': ['tools/bootstrap.cc'],
}

extra_deps = {
  'draw-tree': [cairo],
  'alignment-info': [eigen],
  'alignment-find-conserved': [eigen],
  'alignment-smc': [eigen],
  'joint-parsimony': [eigen],
}

joint_indels_exe = disabler()

python_tool_wrapper_args = []
if host_machine.system() == 'windows' and meson.is_cross_build()
  python_tool_wrapper_args = ['--wrapper'] + windows_test_wrapper
endif

declarative_tool_tests = ['alignment-convert', 'alignment-draw', 'cut-range', 'mcon-tool', 'statreport']
declarative_tool_tests += ['tree-mean-lengths']
tool_test_script = meson.project_source_root()/'tests/tools/run-tests.py'

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

  if tool in declarative_tool_tests
    tool_test_root = meson.project_source_root()/'tests/tools'/tool
    tool_test_names = run_command(
      python, tool_test_script, 'list', tool_test_root, check: true
    ).stdout().strip().split('\n')
    foreach tool_test_name: tool_test_names
      test(tool + ' ' + tool_test_name,
           python,
           args: [tool_test_script, 'run'] + python_tool_wrapper_args
                 + [tool_test_root/tool_test_name, tool_exe],
           depends: tool_exe,
           suite: 'tools')
    endforeach
  endif

  if tool == 'trees-distances'
    trees_distances_test_script = files('../tests/tools/trees-distances/run-tests.py')
    test('trees-distances matching metric',
         python,
         args: [trees_distances_test_script] + python_tool_wrapper_args + [tool_exe],
         depends: tool_exe)
  endif

  if tool == 'alignment-draw'
    node = find_program('node', required: false)
    if node.found()
      alignment_draw_scale_test = files('../tests/tools/alignment-draw/test-scales.js')
      test('alignment-draw scale controls',
           node,
           args: [alignment_draw_scale_test],
           suite: 'tools')
    endif
  endif

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
