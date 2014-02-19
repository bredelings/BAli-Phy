# See libtool documentation on plugins:
#    http://www.flameeyes.eu/autotools-mythbuster/libtool/plugins.html

# http://stackoverflow.com/questions/15454968/dll-plugin-that-uses-functions-defined-in-the-main-executable
# * dlltool --export-all-symbols <program>.exe -l lib<program>.a -D <program>.exe

# 
# http://www.mingw.org/wiki/sampleDLL

AM_CPPFLAGS = -I@top_srcdir@/include -I@top_srcdir@/src

pkglib_LTLIBRARIES = Prelude.la builtinError.la seq.la join.la reapply.la \
		Array.la intToDouble.la doubleToLogDouble.la Alphabet.la \
		iotaUnsigned.la show.la Alignment.la Vector.la \
		c_fst.la c_snd.la c_pair.la evaluate.la Modifiables.la \
		popgen.la Distribution.la Range.la SModel.la trigger.la sum_out_coals.la


if HOST_MINGW32
EXTRADEPENDENCIES = $(top_builddir)/bali-phy.exe
EXTRALDFLAGS = -no-undefined --enable-runtime-pseudo-reloc -L$(top_builddir) -Wl,libbali-phy.dll.a
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

builtinError_la_SOURCES = builtinError.C
builtinError_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

seq_la_SOURCES = seq.C
seq_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

join_la_SOURCES = join.C
join_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

reapply_la_SOURCES = reapply.C
reapply_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Array_la_SOURCES = Array.C
Array_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

intToDouble_la_SOURCES = intToDouble.C
intToDouble_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

doubleToLogDouble_la_SOURCES = doubleToLogDouble.C
doubleToLogDouble_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Prelude_la_SOURCES = Prelude.C
Prelude_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Alphabet_la_SOURCES = Alphabet.C
Alphabet_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

iotaUnsigned_la_SOURCES = iotaUnsigned.C
iotaUnsigned_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_fst_la_SOURCES = c_fst.C
c_fst_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_snd_la_SOURCES = c_snd.C
c_snd_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_pair_la_SOURCES = c_pair.C
c_pair_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

evaluate_la_SOURCES = evaluate.C
evaluate_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Modifiables_la_SOURCES = Modifiables.C
Modifiables_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Alignment_la_SOURCES = Alignment.C
Alignment_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

popgen_la_SOURCES = popgen.C
popgen_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

trigger_la_SOURCES = trigger.C
trigger_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Distribution_la_SOURCES = Distribution.C
Distribution_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Range_la_SOURCES = Range.C
Range_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

SModel_la_SOURCES = SModel.C
SModel_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

show_la_SOURCES = show.C
show_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

sum_out_coals_la_SOURCES = sum_out_coals.C
sum_out_coals_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Vector_la_SOURCES = Vector.C
Vector_la_DEPENDENCIES = $(EXTRADEPENDENCIES)
