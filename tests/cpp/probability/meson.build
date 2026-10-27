probability_tests = executable(
  'probability-tests',
  'choice-weight-tests.cc',
  build_rpath: build_rpath,
  include_directories: [root_inc, libutil_inc, native_test_inc],
  dependencies: [boost, eigen, libdl, math_dep, immer, fmt, cereal],
  link_with: [libbaliphy, libutil],
)

test('probability choices', probability_tests)
