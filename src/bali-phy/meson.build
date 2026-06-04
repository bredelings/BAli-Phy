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

baliphy_link_args = []
if host_machine.system() == 'windows'
  if cpp.get_id() == 'msvc' or cpp.get_id() == 'clang-cl'
    baliphy_link_args += ['/STACK:8388608']
  else
    baliphy_link_args += ['-Wl,--stack,8388608']
  endif
endif

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: [root_inc, libutil_inc],
		     dependencies: [boost, eigen, libdl, math_dep, librange, fmt, cereal, immer],
		     link_whole: [libutil, libmcon, libcommon, libcomputation, libbaliphy],
		     link_args: baliphy_link_args,
		     install_rpath: extra_rpath,
		     install: true,
		     export_dynamic: true)


