boost_inc = include_directories('include', is_system:true)

subdir('lib')

boost = declare_dependency(include_directories: boost_inc, link_with: boost_libs, dependencies: threads).as_system()
