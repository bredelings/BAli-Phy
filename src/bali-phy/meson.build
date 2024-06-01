baliphy_sources = [
  'link-partitions.cc',
  'bali-phy.cc',
  'help.cc',
  'A-T-model.cc',
  'files.cc',
  'system.cc',
  'cmd_line.cc',
  'paths.cc']

# maybe we don't need this if we are doing static linking
extra_rpath = ''
if host_machine.system() == 'linux'
  extra_rpath = '$ORIGIN/../lib/bali-phy/'
endif

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: [root_inc],
		     dependencies: [boost, eigen, libdl, math_dep, librange, fmt, util],
		     link_whole: [libutil, libmcon, libcommon, libcomputation, libbaliphy],
		     install_rpath: extra_rpath,
		     install: true,
		     export_dynamic: true)



