model_tests = executable(
  'model-tests',
  'model-expression-tests.cc',
  include_directories: [root_inc, libutil_inc],
  dependencies: [boost, libxxhash, eigen, libdl, math_dep, librange, immer, fmt, cereal],
  link_with: [libbaliphy, libcomputation, libcommon, libmcon, libutil],
)

test('models', model_tests)
