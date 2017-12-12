# https://github.com/mesonbuild/meson/blob/master/docs/markdown/Porting-from-autotools.md
# http://mesonbuild.com/Syntax.html
# http://mesonbuild.com/howtox.html#set-default-cc-language-version
project('bali-phy', 'cpp',
	version: '3.0-beta5',
	default_options : [
	  'buildtype=release',
	  'cpp_std=c++14'
	],
	meson_version: '>= 0.43',
	license: 'GPLv2')

cpp = meson.get_compiler('cpp')

# put this in a config file
add_project_arguments('-DPACKAGE_VERSION="'+meson.project_version()+'"', language : 'cpp')
add_project_arguments('-D_ARCH_="'+host_machine.system()+' ' + host_machine.cpu_family() + '"', language : 'cpp')
add_project_arguments('-D_COMPILER_="'+cpp.get_id() + ' ' + cpp.version()+' ' + host_machine.cpu_family() + '"', language : 'cpp')

add_project_arguments(['-Wall','-Wextra','-Wno-sign-compare','-Woverloaded-virtual','-Wstrict-aliasing'], language : 'cpp')

if get_option('buildtype') == 'release'
  add_project_arguments(['-DNDEBUG','-DNDEBUG_DP','-funroll-loops','-ffast-math'], language : 'cpp')
endif

if host_machine.system() == 'darwin'
  add_project_arguments('-fvisibility=default', language : 'cpp')
endif

eigen = include_directories('include', is_system: true)

cairo = dependency('cairo', required: false)

install_data('scripts/bp-analyze', install_mode: 'rwxr-----', install_dir: 'bin')
install_data(['scripts/compare-runs.R', 'scripts/compare-runs2.R', 'scripts/tree-plot1.R',
	      'scripts/tree-plot2.R', 'scripts/tree-plot3.R', 'scripts/tree-plot2-3D.R',
	      'scripts/tree-plot1-3D.R', 'scripts/tree-plot3-3D.R'],
	     install_dir:'libexec/bali-phy')
install_data(['doc/README.html','doc/README.xhtml','doc/README.pdf'], install_dir: 'share/doc/bali-phy')

## we need to build libdl if we are on windows (cygwin or mingw64, and then find the library
install_subdir('modules', install_dir: 'lib/bali-phy')
install_subdir('functions', install_dir: 'lib/bali-phy')
install_subdir('help', install_dir: 'lib/bali-phy')
install_subdir('examples', install_dir: 'share/bali-phy')

# for boost and eigen, we really want something like yes, no, auto* -- default to auto.
# if we have 'auto', then build our own boost if we want stl-debug and bounds_checking
boost_modules = [ 'program_options', 'random', 'system','filesystem', 'chrono']
if get_option('with-mpi')
  boost_modules += ['serialization', 'mpi']
endif

if get_option('with-system-boost')
  boost = dependency('boost', modules : boost_modules)
else
  subdir('boost')
endif

if get_option('with-system-eigen')
  eigen = include_directories('')
else
  eigen = include_directories('include', is_system: true)
endif

if get_option('with-mpi')
endif

subdir('src')

python = find_program('python')
tests = join_paths(meson.source_root(),'tests')
run_tests = files('tests/run-tests.py')
baliphyexe = join_paths(meson.build_root(),'src/bali-phy')
builtins = join_paths(meson.build_root(),'src/builtins')

test('internal testsuite', python,
     args: [run_tests, baliphyexe, '--package-path=@0@:@1@'.format(builtins,meson.source_root())],
     workdir: tests)
