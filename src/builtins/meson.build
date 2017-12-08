source_inc = include_directories('..')

builtins = ['Array','Data','Prelude','Alphabet','Pair','Modifiables','Alignment',
	    'PopGen','Distribution','Range','SModel','MCMC','Vector','Environment']

implibs = []
if host_machine.system() == 'windows'
  implibs += baliphy
endif

foreach mod : builtins
  shared_module(mod,
		mod + '.cc',
		include_directories: source_inc,
		link_with: implibs,
		install_dir: 'lib/bali-phy',
		install: true)
endforeach
