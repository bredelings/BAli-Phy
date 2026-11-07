slice_sampling_tests = executable(
  'slice-sampling-tests',
  'slice-sampling-tests.cc',
  build_rpath: build_rpath,
  include_directories: [root_inc, libutil_inc, native_test_inc],
  dependencies: [allocator_dep, boost, libxxhash, eigen, libdl, math_dep, immer, fmt, cereal],
  link_with: [libbaliphy, libcomputation, libcommon, libmcon, libutil],
)

test('slice sampling boundaries', slice_sampling_tests)
