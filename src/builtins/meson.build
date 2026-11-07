source_inc = include_directories('../..')

builtins = ['Data','Prelude','Alphabet','Pair','Modifiables','Alignment',
            'PopGen','Distribution','Range','SModel','MCMC','Vector','Environment',
            'SMC','Real','Bits','Foreign','Text','ByteString','Char', 'Num', 'IntMap', 'IntSet',
            'Word', 'Parsimony', 'NativeVector', 'Matrix', 'Likelihood', 'LikelihoodSEV', 'File', 'TreeDist']

implibs = []
if host_machine.system() == 'windows' or host_machine.system() == 'cygwin'
  implibs += baliphy
  extension = 'dll'
else
  extension = 'so'
endif

builtin_rpath = ''
if host_machine.system() == 'darwin'
  builtin_rpath = '@loader_path'
elif host_machine.system() == 'linux'
  builtin_rpath = '$ORIGIN'
endif

foreach mod : builtins
  shared_module(mod,
		mod + '.cc',
		include_directories: [source_inc, root_inc, libmcon_inc, libutil_inc],
		dependencies: [allocator_dep, boost, eigen, immer, cereal],
		link_with: implibs,
		install_dir: 'lib/bali-phy',
		install_rpath: builtin_rpath,
		name_prefix: '',
		name_suffix: extension,
		install: true)
endforeach
