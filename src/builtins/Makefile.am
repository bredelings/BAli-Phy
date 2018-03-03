# See libtool documentation on plugins:
#    http://www.flameeyes.eu/autotools-mythbuster/libtool/plugins.html

# http://stackoverflow.com/questions/15454968/dll-plugin-that-uses-functions-defined-in-the-main-executable
# * dlltool --export-all-symbols <program>.exe -l lib<program>.a -D <program>.exe

# 
# http://www.mingw.org/wiki/sampleDLL

AM_CPPFLAGS = -I@top_srcdir@/include -I@top_srcdir@/src

pkglib_LTLIBRARIES = Prelude.la Array.la Alphabet.la \
		Alignment.la Vector.la Pair.la Modifiables.la \
		PopGen.la Distribution.la Range.la SModel.la MCMC.la \
		Environment.la Data.la SMC.la


if HOST_WIN_ALL
EXTRADEPENDENCIES = $(top_builddir)/src/bali-phy.exe
EXTRALDFLAGS = -no-undefined --enable-runtime-pseudo-reloc  -Wl,$(top_builddir)/src/libbali-phy.dll.a
else
if HOST_DARWIN
# I think -no-undefined leads to the creation of bundles, which then need a bundle_loader argument
# See http://www.opensource.apple.com/source/X11server/X11server-46/xorg-server-X11R7.2-1.2.0/hw/darwin/Makefile.am 
#  for a possible example.
EXTRADEPENDENCIES = # -no-undefined <?? -XCClinker -bundle_loader -XCClinker /path/to/bali-phy ??>
EXTRALDFLAGS =
else
EXTRADEPENDENCIES =
EXTRALDFLAGS =
endif
endif

LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)

Array_la_SOURCES = Array.cc
Array_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Data_la_SOURCES = Data.cc
Data_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Prelude_la_SOURCES = Prelude.cc
Prelude_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Alphabet_la_SOURCES = Alphabet.cc
Alphabet_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Pair_la_SOURCES = Pair.cc
Pair_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Modifiables_la_SOURCES = Modifiables.cc
Modifiables_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Alignment_la_SOURCES = Alignment.cc
Alignment_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

PopGen_la_SOURCES = PopGen.cc
PopGen_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Distribution_la_SOURCES = Distribution.cc
Distribution_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Range_la_SOURCES = Range.cc
Range_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

SModel_la_SOURCES = SModel.cc
SModel_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

MCMC_la_SOURCES = MCMC.cc
MCMC_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Vector_la_SOURCES = Vector.cc
Vector_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Environment_la_SOURCES = Environment.cc
Environment_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

SMC_la_SOURCES = SMC.cc
SMC_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

