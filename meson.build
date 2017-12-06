# https://github.com/mesonbuild/meson/blob/master/docs/markdown/Porting-from-autotools.md
# http://mesonbuild.com/Syntax.html
# http://mesonbuild.com/howtox.html#set-default-cc-language-version
project('bali-phy', 'cpp', version: '3.0-beta5', license: 'GPLv2', default_options : ['cpp_std=c++14'])

add_global_arguments(['-Wall','-Wextra','-Wno-sign-compare','-Woverloaded-virtual','-Wstrict-aliasing'], language : 'cpp')
#glib_dep = dependency('glib-2.0')

eigen = include_directories('include')


pkg_mod = import('pkgconfig')
#pkg_mod.generate(libraries : foolib,
#                 version : '1.0',
#                 name : 'libfoobar',
#                 filebase : 'foobar',
#                 description : 'A Library to barnicate your foos.')

subdir('src')


