reflex_inc = include_directories('include', is_system: true)

# Use upstream's portable scanner runtime, without generator-only Unicode tables.
# BAli-Phy's HAVE_CONFIG_H must not alter RE-flex's unconfigured platform defaults.
reflex_lib = static_library('reflex_runtime',
  'lib/debug.cpp',
  'lib/error.cpp',
  'lib/input.cpp',
  'lib/matcher.cpp',
  'lib/pattern.cpp',
  'lib/utf8.cpp',
  'lib/simd.cpp',
  include_directories: reflex_inc,
  cpp_args: ['-UHAVE_CONFIG_H'],
  install: false)

reflex_dep = declare_dependency(
  include_directories: reflex_inc,
  link_with: reflex_lib)

install_data('LICENSE.txt',
  install_dir: get_option('datadir') / 'doc' / 'bali-phy' / 'licenses' / 're-flex')
