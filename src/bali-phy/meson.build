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
if target_machine.system() == 'linux'
  extra_rpath = '$ORIGIN/../lib/bali-phy/'
elif target_machine.system() == 'darwin'
  # @loader_path should be the same as @executable_path for executables
  extra_rpath = '@executable_path/../lib/bali-phy/'
endif

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: [root_inc, libutil_inc],
		     dependencies: [boost, eigen, libdl, math_dep, librange, fmt, cereal, immer, libxxhash],
		     link_whole: [libutil, libmcon, libcommon, libcomputation, libbaliphy],
		     install_rpath: extra_rpath,
		     install: true,
		     export_dynamic: true)



