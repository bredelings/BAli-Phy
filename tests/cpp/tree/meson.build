tree_tests = executable(
  'tree-tests',
  'tree-tests.cc',
  build_rpath: build_rpath,
  include_directories: [root_inc, libutil_inc],
  dependencies: [boost, libxxhash, eigen, math_dep, librange],
  link_with: [libcommon, libmcon, libutil],
)

test('tree operations', tree_tests)
