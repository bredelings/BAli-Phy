project('reflex', 'cpp',
  version: '6.3.0',
  license: 'BSD-3-Clause',
  meson_version: '>= 1.1',
  default_options: ['cpp_std=c++11'])

reflex_inc = include_directories('include')

reflex_sources = files(
  'lib/convert.cpp',
  'lib/debug.cpp',
  'lib/error.cpp',
  'lib/input.cpp',
  'lib/matcher.cpp',
  'lib/matcher_avx2.cpp',
  'lib/matcher_avx512bw.cpp',
  'lib/pattern.cpp',
  'lib/posix.cpp',
  'lib/simd.cpp',
  'lib/simd_avx2.cpp',
  'lib/simd_avx512bw.cpp',
  'lib/unicode.cpp',
  'lib/utf8.cpp',
  'unicode/block_scripts.cpp',
  'unicode/language_scripts.cpp',
  'unicode/letter_scripts.cpp',
  'unicode/letter_case.cpp',
  'unicode/composer.cpp')

# Compatibility note: this overlay intentionally starts with RE/flex's
# portable build path. Add Meson SIMD feature detection only if profiling shows
# that scanner generation or runtime matching needs it.
reflex_lib = static_library('reflex_static_lib',
  reflex_sources,
  include_directories: reflex_inc,
  install: false)

reflex_dep = declare_dependency(
  include_directories: reflex_inc,
  link_with: reflex_lib)

reflex = executable('reflex',
  'src/reflex.cpp',
  include_directories: reflex_inc,
  link_with: reflex_lib,
  install: false)

meson.override_dependency('reflex', reflex_dep)
