# https://github.com/mesonbuild/meson/blob/master/docs/markdown/Porting-from-autotools.md
# http://mesonbuild.com/Syntax.html
# http://mesonbuild.com/howtox.html#set-default-cc-language-version
project('bali-phy', ['cpp','c'],
	version: '3.0',
	default_options : [
	  'buildtype=release',
	  'cpp_std=c++14'
	],
	meson_version: '>= 0.44',
	license: 'GPLv2')

cpp = meson.get_compiler('cpp')
cc = meson.get_compiler('c')

# 2. Write a 'config.h'
conf_data = configuration_data()

# 2.1. Write some info to 'config.h'
conf_data.set_quoted('PACKAGE_VERSION', meson.project_version())
conf_data.set_quoted('_ARCH_', host_machine.system()+' ' + host_machine.cpu_family())
conf_data.set_quoted('_COMPILER_', cpp.get_id() + ' ' + cpp.version()+' ' + host_machine.cpu_family())

# 2.2. Write header and function checks to'config.h'
conf_data.set('HAVE_SYS_RESOURCE_H', cpp.has_header('sys/resource.h'))
conf_data.set('HAVE_FENV_H', cpp.has_header('fenv.h'))
conf_data.set('HAVE_LOG_2', cpp.has_function('log2'))
conf_data.set('HAVE_FEENABLEEXCEPT', cpp.has_function('feenableexcept'))

# 2.3. Write config file, get root_inc to include it, and tell the compiler it exists.
configure_file(output : 'config.h', configuration : conf_data)
root_inc = include_directories('.')
add_project_arguments('-DHAVE_CONFIG_H', language : 'cpp')

add_project_arguments(['-Wall','-Wextra','-Wno-sign-compare','-Woverloaded-virtual','-Wstrict-aliasing','-Wno-unknown-pragmas'], language : 'cpp')

if cpp.has_argument('-Wno-maybe-uninitialized')
  add_project_arguments(['-Wno-maybe-uninitialized'], language: 'cpp')
endif

if get_option('buildtype') == 'release' or get_option('buildtype') == 'plain'
  add_project_arguments(['-DNDEBUG','-DNDEBUG_DP','-O3','-funroll-loops','-ffast-math'], language : 'cpp')
endif

if host_machine.system() == 'darwin'
  add_project_arguments('-fvisibility=default', language : 'cpp')
endif

cairo = dependency('cairo', required: false)
if not cairo.found()
  cairo = disabler()
endif

install_data('scripts/bp-analyze', install_mode: 'rwxr-xr-x', install_dir: 'bin')
install_data(['scripts/compare-runs.R', 'scripts/compare-runs2.R', 'scripts/tree-plot1.R',
	      'scripts/tree-plot2.R', 'scripts/tree-plot3.R', 'scripts/tree-plot2-3D.R',
	      'scripts/tree-plot1-3D.R', 'scripts/tree-plot3-3D.R'],
	     install_dir:'lib/bali-phy/libexec')
install_data(['doc/README.html','doc/README.xhtml','doc/README.pdf'], install_dir: 'share/doc/bali-phy')

## we need to build libdl if we are on windows (cygwin or mingw64, and then find the library
install_subdir('modules', install_dir: 'lib/bali-phy')
install_subdir('functions', install_dir: 'lib/bali-phy')
install_subdir('help', install_dir: 'lib/bali-phy')
install_subdir('examples', install_dir: 'share/bali-phy')

if get_option('with-mpi')
  mpi = dependency('mpi', language='cpp')
endif

# for boost and eigen, we really want something like yes, no, auto* -- default to auto.
# if we have 'auto', then build our own boost if we want stl-debug and bounds_checking
boost_modules = [ 'program_options', 'random', 'system','filesystem', 'chrono']
if get_option('with-mpi')
  boost_modules += ['serialization', 'mpi']
endif

if get_option('with-system-boost')
  boost = dependency('boost', modules : boost_modules)
else
  subdir('external/boost')
endif


if get_option('with-system-eigen')
  eigen = dependency('eigen3')
else
  eigen = include_directories('external/eigen3', is_system: true)
  eigen = declare_dependency(include_directories: eigen)
endif

json = include_directories('external/nlohmann', is_system: true)
json = declare_dependency(include_directories: json)


subdir('src')

run_tests = files('scripts/run-tests.sh')
baliphyexe = join_paths(meson.build_root(),'src/bali-phy')
builtins = join_paths(meson.build_root(),'src/builtins')
packagepath = '--package-path=@0@:@1@'.format(builtins,meson.source_root())

check = run_target('check', command: [run_tests, baliphyexe, packagepath])
