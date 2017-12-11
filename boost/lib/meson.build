# FIXME: I think this is a link_with, not a dependency...
boost = []

foreach module : boost_modules
  subdir(module)
endforeach

boost = dependency('boost', modules : boost_modules)
