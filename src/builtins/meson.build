source_inc = include_directories('../..')

builtins = ['Array','Data','Prelude','Alphabet','Pair','Modifiables','Alignment',
	    'PopGen','Distribution','Range','SModel','MCMC','Vector','Environment',
            'SMC','Real','Bits','Foreign','Text','Char', 'Num', 'IntMap', 'IntSet',
            'Parsimony', 'Matrix', 'Likelihood', 'File']

implibs = []
if host_machine.system() == 'windows' or host_machine.system() == 'cygwin'
  implibs += baliphy
  extension = 'dll'
else
  extension = 'so'
endif

foreach mod : builtins
  shared_module(mod,
		mod + '.cc',
		include_directories: [libutil_inc, source_inc, root_inc],
		dependencies: [boost, json, eigen, immer],
		link_with: implibs,
		install_dir: 'lib/bali-phy',
		name_prefix: '',
		name_suffix: extension,
		install: true)
endforeach
