util_tests = executable(
  'util-tests',
  'text-tests.cc',
  include_directories: libutil_inc,
  dependencies: [boost, eigen, librange, fmt, cereal, utf8proc_dep],
  link_with: libutil,
)

test('util', util_tests)
