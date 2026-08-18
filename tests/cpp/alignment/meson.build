alignment_sample_file_tests = executable(
  'alignment-sample-file-tests',
  'character-property-sample-file-tests.cc',
  include_directories: [root_inc, libutil_inc],
  dependencies: [boost, libxxhash, eigen, math_dep, librange],
  link_with: [libcommon, libmcon, libutil],
)

test('character property sample file snapshots', alignment_sample_file_tests)
