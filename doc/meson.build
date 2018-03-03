install_data(['README.html','README.xhtml','README.pdf'], install_dir: join_paths(get_option('datadir'),'doc/bali-phy'))

pandoc = find_program('pandoc', required: false)

if pandoc.found()
  foreach prog : all_progs
    markdown = files('man/'+prog+'.md')
    manpage = prog+'.1'
    custom_target(manpage,command:[pandoc,'-s','-t','man',markdown], output:manpage, capture: true, install: true, install_dir: join_paths(get_option('mandir'),'man1'))
  endforeach
else
  warning('Program "pandoc" not found!  Cannot generate man pages')
endif
