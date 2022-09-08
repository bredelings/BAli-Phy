project('bali-phy', ['cpp','c'],
        version: '4.0-alpha8pre',
        default_options : [
          'cpp_std=c++20',
          'warning_level=3',
          'buildtype=release',
          'b_ndebug=if-release'
        ],
        meson_version: '>= 0.56',
        license: 'GPLv2')

cpp = meson.get_compiler('cpp')
cc = meson.get_compiler('c')

# 2. Write a 'config.h'
conf_data = configuration_data()

# 2.1. Write some info to 'config.h'
conf_data.set_quoted('PACKAGE_VERSION', meson.project_version())
conf_data.set_quoted('_ARCH_', host_machine.system()+' ' + host_machine.cpu_family())
conf_data.set_quoted('_COMPILER_', cpp.get_id() + ' ' + cpp.version() + ' ' + host_machine.cpu_family())

# 2.2. Write header and function checks to'config.h'
conf_data.set('HAVE_SYS_RESOURCE_H', cpp.has_header('sys/resource.h'))
conf_data.set('HAVE_FENV_H', cpp.has_header('fenv.h'))
conf_data.set('HAVE_LOG_2', cpp.has_function('log2'))
conf_data.set('HAVE_FEENABLEEXCEPT', cpp.has_function('feenableexcept'))

# 2.3. Write config file, get root_inc to include it, and tell the compiler it exists.
configure_file(output : 'config.h', configuration : conf_data)
root_inc = include_directories('.','src')
add_project_arguments('-DHAVE_CONFIG_H', language : 'cpp')


# 3. Set compiler flags

# 3.1 Set warning flags
warn_flags = ['-Wno-sign-compare','-Wno-maybe-uninitialized','-Woverloaded-virtual','-Wstrict-aliasing','-Wno-unknown-pragmas', '-fdiagnostics-show-template-tree']

add_project_arguments(cpp.get_supported_arguments(warn_flags), language : 'cpp')

# 3.2 Set optimization flags

if get_option('optimization') == '2' or get_option('optimization') == '3'
  opt_flags = ['-funroll-loops','-fno-math-errno','-fno-signed-zeros']
  add_project_arguments(cpp.get_supported_arguments(opt_flags), language : 'cpp')
endif

# 3.3 Enable/Disable extra debugging for dynamic programming
assertions_off = get_option('b_ndebug') == 'true' or (get_option('b_ndebug') == 'if-release' and
                                                      (get_option('buildtype') == 'release' or
                                                       get_option('buildtype') == 'plain')
                                                     )
if assertions_off
  add_project_arguments('-DNDEBUG_DP', language : 'cpp')
endif

# 4. Dependencies

threads = dependency('threads')

if host_machine.system() == 'darwin'
  add_project_arguments('-fvisibility=default', language : 'cpp')
endif

cairo = dependency('cairo', required: false)
if not cairo.found()
  used_cairo = 'not found'
  cairo = disabler()
else
  used_cairo = 'system'
endif

if get_option('with-mpi')
  mpi = dependency('mpi', language='cpp')
endif

boost_modules = [ 'program_options', 'random', 'system', 'chrono']
if get_option('with-mpi')
  boost_modules += ['serialization', 'mpi']
endif

# FIXME: configure these things as sub-projects
# FIXME: Build our own boost if cpp_debug_stl
# FIXME: Only do the 'auto' approach (basically fallback)?
# FIXME: Perhaps use our own if we think the system one won't work

## Handle auto / system / internal method of finding/building BOOST
used_boost = get_option('boost')
if get_option('boost') == 'auto'
  boost = dependency('boost', modules : boost_modules, version: '>=1.63', required: false)
  if not boost.found()
    message('Building internal BOOST libraries.')
    subdir('external/boost')
    used_boost = 'internal'
  else
    used_boost = 'system'
  endif
elif get_option('boost') == 'system'
  boost = dependency('boost', modules : boost_modules)
else
  subdir('external/boost')
endif

## Handle auto / system / internal method of finding/building EIGEN
used_eigen = get_option('eigen')
if get_option('eigen') == 'auto'
  eigen = dependency('eigen3', required: false).as_system()
  if not eigen.found()
    used_eigen = 'internal'
    message('Using internal copy of eigen.')
    eigen = include_directories('external/eigen3', is_system: true)
    eigen = declare_dependency(include_directories: eigen)
  else
    used_eigen = 'system'
  endif
elif get_option('eigen') == 'system'
  eigen = dependency('eigen3').as_system()
else
  eigen = include_directories('external/eigen3', is_system: true)
  eigen = declare_dependency(include_directories: eigen)
endif

## Handle auto / system / internal method of finding  <nlohmann/json.hpp>
used_json = get_option('json')
if get_option('json') == 'auto'
  if cpp.has_header('nlohmann/json.hpp')
    used_json = 'system'
    json = []
  else
    used_json = 'internal'
    message('Using internal copy of <nlohman/json.hpp>.')
    json = include_directories('external/nlohmann', is_system: true)
    json = declare_dependency(include_directories: json)
  endif
elif get_option('json') == 'system'
  if cpp.has_header('nlohmann/json.hpp')
    json = []
  else
    error('No <nlohmann/json.hpp> header could be found!')
  endif
else
  json = include_directories('external/nlohmann', is_system: true)
  json = declare_dependency(include_directories: json)
endif

if host_machine.system() == 'windows'
  # We should have a system of 'cygwin' for cygwin.
  subdir('external/dlfcn-win32')
else
  libdl = cpp.find_library('dl')
endif

if cpp.has_header('range/v3/view/take.hpp')
  used_librange = 'system'
  librange = []
else
  used_librange = 'internal'
  subdir('external/range-v3/')
endif

subdir('external/immer/')

math_dep = cpp.find_library('m', required:false)

small_fasta=files('examples/sequences/5S-rRNA/5d.fasta')
big_fasta=files('examples/sequences/5S-rRNA/48-muscle.fasta')
builtins = meson.project_build_root()/'src/builtins'
packagepath = '--package-path=@0@:@1@'.format(builtins,meson.project_source_root())

subdir('src')

subdir('doc')

# Run internal tests
run_tests = find_program(meson.project_source_root()/'tests/run-tests.py')

test('bali-phy testsuite',
     run_tests,
     timeout: 3000,
     workdir: meson.project_source_root()/'tests',
     args:[baliphy.full_path(), packagepath])

# Install non-executable files

install_data('scripts/bp-analyze', install_mode: 'rwxr-xr-x', install_dir: 'bin')
install_data('scripts/bali-phy-pkg', install_mode: 'rwxr-xr-x', install_dir: 'bin')
install_data(['scripts/compare-runs.R', 'scripts/compare-runs2.R',
              'scripts/tree-plot.R', 'scripts/tree-plot-3D.R'],
             install_dir:'lib/bali-phy/libexec')

install_subdir('haskell', install_dir: 'lib/bali-phy')
install_subdir('bindings', install_dir: 'lib/bali-phy')
install_subdir('help', install_dir: 'lib/bali-phy')
install_subdir('examples', install_dir: get_option('datadir')/'doc/bali-phy')

# Print a summary of the configuration

summary({'host': host_machine.system()
        }, section: 'Architecture')

summary({'BOOST': used_boost,
         'Eigen': used_eigen,
         'nlohmann::json': used_json,
         'range v3': used_librange,
         'cairo': used_cairo
        },section: 'Libraries')

summary({'prefix': get_option('prefix'),
        },section: 'Directories')

assertions_enabled = 'enabled'
if assertions_off
  assertions_enabled = 'disabled'
endif

summary({'optimization': get_option('optimization'),
         'debug': get_option('debug'),
         'assertions': assertions_enabled,
        },section: 'Configuration')
