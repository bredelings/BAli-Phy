slice_sampling_tests = executable(
  'slice-sampling-tests',
  'slice-sampling-tests.cc',
  include_directories: [root_inc, libutil_inc, native_test_inc],
  dependencies: [boost, libxxhash, eigen, libdl, math_dep, librange, immer, fmt, cereal],
  link_with: [libbaliphy, libcomputation, libcommon, libmcon, libutil],
)

test('slice sampling boundaries', slice_sampling_tests)
