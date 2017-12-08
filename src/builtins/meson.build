source_inc = include_directories('..')

builtins = ['Array','Data','Prelude','Alphabet','Pair','Modifiables','Alignment',
	    'PopGen','Distribution','Range','SModel','MCMC','Vector','Environment']

foreach mod : builtins
  shared_module(mod,
		mod + '.cc',
		include_directories: source_inc,
		link_with: baliphy,
		install_dir: 'lib/bali-phy',
		install: true)
endforeach
