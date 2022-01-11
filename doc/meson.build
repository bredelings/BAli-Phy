pandoc = find_program('pandoc', required: false)

if pandoc.found()
  pandoc_cmd = [pandoc, '-s', '-t', 'man']

  # -f markdown-smart is necessary to avoid changing -- to a single en-dash
  # But its only present in version 2 or higher.
  pandoc_version = run_command(pandoc,['--version'], check: true).stdout().split('\n')[0].split(' ')[1].split('.')[0]
  if pandoc_version == '2'
    pandoc_cmd = pandoc_cmd + ['-f', 'markdown-smart']
  endif

  foreach prog : all_progs+['bp-analyze', 'bali-phy-pkg']
    markdown = files('man/'+prog+'.md')
    manpage = prog+'.1'
    custom_target(manpage, command:pandoc_cmd + [markdown], output:manpage, capture: true, install: true, install_dir: join_paths(get_option('mandir'),'man1'))
  endforeach

else
  warning('Program "pandoc" not found!  Cannot generate man pages')
endif

