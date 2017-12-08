# https://github.com/mesonbuild/meson/blob/master/docs/markdown/Porting-from-autotools.md
# http://mesonbuild.com/Syntax.html
# http://mesonbuild.com/howtox.html#set-default-cc-language-version
project('bali-phy', 'cpp', version: '3.0-beta5', license: 'GPLv2', default_options : ['cpp_std=c++14'])

add_global_arguments('-DPACKAGE_VERSION="'+meson.project_version()+'"', language : 'cpp')
add_global_arguments(['-Wall','-Wextra','-Wno-sign-compare','-Woverloaded-virtual','-Wstrict-aliasing'], language : 'cpp')

cpp = meson.get_compiler('cpp')

eigen = include_directories('include', is_system: true)

cairo = dependency('cairo', required: false)

## we need to build libdl if we are on windows (cygwin or mingw64, and then find the library
libdl = cpp.find_library('dl')

subdir('src')


