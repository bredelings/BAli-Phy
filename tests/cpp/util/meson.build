util_tests = executable(
  'util-tests',
  'text-tests.cc',
  build_rpath: build_rpath,
  include_directories: libutil_inc,
  dependencies: [boost, eigen, librange, fmt, cereal, utf8proc_dep],
  link_with: libutil,
)

test('util', util_tests)
