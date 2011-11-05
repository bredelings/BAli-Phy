add_library(boost_chrono
  src/chrono.cpp
  src/thread_clock.cpp
  src/process_cpu_clocks.cpp
)

if (HOST_LINUX)
  target_link_libraries(boost_chrono rt)
endif()

