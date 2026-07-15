computation_tests = executable(
  'computation-tests',
  [
    'computation-test-main.cc',
    'closure-tests.cc',
    'serialization-tests.cc',
    'transform-tests.cc',
    'values-tests.cc',
  ],
  include_directories: [root_inc, libutil_inc, libmcon_inc, native_test_inc],
  dependencies: [boost, libxxhash, eigen, libdl, math_dep, librange, immer, fmt, cereal],
  link_with: [libcomputation, libcommon, libmcon, libutil],
  export_dynamic: true,
)

if host_machine.system() == 'windows'
  computation_test_runner = meson.current_source_dir()/'run-on-windows.py'
  computation_test_args = [
    computation_tests.full_path(),
    builtins_path,
    meson.project_source_root(),
  ]
  if meson.is_cross_build()
    computation_test_args = ['--wrapper', 'wine'] + computation_test_args
  endif
  test(
    'computation',
    python,
    args: [computation_test_runner] + computation_test_args,
    depends: computation_tests,
    timeout: 120,
  )
else
  test(
    'computation',
    computation_tests,
    args: [builtins_path, meson.project_source_root()],
  )
endif
