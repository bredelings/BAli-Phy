chrono_sources = ['src/chrono.cpp', 'src/thread_clock.cpp', 'src/process_cpu_clocks.cpp']

rt_lib = []
if host_machine.system() == 'linux'
  rt_lib = cpp.find_library('rt')
endif

boost_libs += static_library('boost_chrono', chrono_sources,
			     include_directories: boost_inc,
			     dependencies: rt_lib)
