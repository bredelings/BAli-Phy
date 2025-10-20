project('bali-phy', ['cpp','c'],
        version: '4.2-preview',
        default_options : [
          'cpp_std=c++23',
          'warning_level=3',
          'buildtype=release',
          'b_ndebug=if-release'
        ],
        meson_version: '>= 1.1',
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

boost_modules = [ 'program_options', 'random', 'chrono', 'json']
if get_option('with-mpi')
  boost_modules += ['serialization', 'mpi']
endif

# NOTE: we can't use cpp_debug_stl unless we build our own BOOST.
# But we don't have to build it inside of bali-phy.

boost = dependency('boost', version: '>=1.81.0', modules : boost_modules).as_system()

## Handle auto / system / internal method of finding/building EIGEN
eigen = dependency('eigen3').as_system()

fmt = dependency('fmt', static: true).as_system()

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

cereal = dependency('cereal').as_system()

subdir('external/immer/')

math_dep = cpp.find_library('m', required:false)

small_fasta=files('examples/5S-rRNA/5d-muscle.fasta')
big_fasta=files('examples/5S-rRNA/48-muscle.fasta')
builtins = meson.project_build_root()/'src/builtins'
packagepath = '--package-path=@0@:@1@'.format(builtins,meson.project_source_root())

subdir('src')

subdir('doc')

# Run internal tests
run_tests = find_program(meson.project_source_root()/'tests/run-tests.py')
test_dir = meson.project_source_root()/'tests'

wrapper = []
if host_machine.system() == 'windows'
  wrapper = ['wine']
endif

test_list = run_command(run_tests, 'listdir', test_dir, baliphy.full_path(), check: false).stdout().split()
foreach test: test_list
  test('bali-phy testsuite ' + test,
       run_tests,
       timeout: 300,
       workdir: test_dir,
       args:['run', test, wrapper, baliphy.full_path(), packagepath])
endforeach

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

summary({'arch': host_machine.cpu_family(),
         'host': host_machine.system(),
        }, section: 'Architecture')

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

summary({
         'range v3': used_librange,
         'cairo': used_cairo
        },section: 'Libraries')

