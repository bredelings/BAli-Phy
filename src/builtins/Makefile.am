# See libtool documentation on plugins:
#    http://www.flameeyes.eu/autotools-mythbuster/libtool/plugins.html

# http://stackoverflow.com/questions/15454968/dll-plugin-that-uses-functions-defined-in-the-main-executable
# * dlltool --export-all-symbols <program>.exe -l lib<program>.a -D <program>.exe

# 
# http://www.mingw.org/wiki/sampleDLL


pkglib_LTLIBRARIES = mod.la Prelude.la builtinError.la seq.la join.la reapply.la \
		arraySize.la getIndex.la mkArray.la intToDouble.la negate.la \
		exp.la doubleToLogDouble.la \
		equals.la notequals.la greaterthan.la greaterthanorequal.la lessthan.la \
		lessthanorequal.la getNucleotides.la plus_gwF.la alphabetSize.la \
		iotaUnsigned.la f3x4_frequencies.la gtr.la m0.la \
		set_vector_index.la new_vector.la get_vector_index.la vector_size.la \
		c_fst.la c_snd.la c_pair.la evaluate.la Modifiables.la\
		show.la rs07_branch_HMM.la rs07_lengthp.la pairwise_alignment_length1.la \
		pairwise_alignment_length2.la transition_counts.la pairwise_alignment_probability_from_counts.la \
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

mod_la_SOURCES = computation/builtins/mod.C
mod_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
mod_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

builtinError_la_SOURCES = computation/builtins/builtinError.C
builtinError_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
builtinError_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

seq_la_SOURCES = computation/builtins/seq.C
seq_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
seq_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

join_la_SOURCES = computation/builtins/join.C
join_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
join_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

reapply_la_SOURCES = computation/builtins/reapply.C
reapply_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
reapply_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

arraySize_la_SOURCES = computation/builtins/arraySize.C
arraySize_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
arraySize_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

getIndex_la_SOURCES = computation/builtins/getIndex.C
getIndex_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
getIndex_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

mkArray_la_SOURCES = computation/builtins/mkArray.C
mkArray_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
mkArray_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

intToDouble_la_SOURCES = computation/builtins/intToDouble.C
intToDouble_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
intToDouble_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

doubleToLogDouble_la_SOURCES = computation/builtins/doubleToLogDouble.C
doubleToLogDouble_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
doubleToLogDouble_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

negate_la_SOURCES = computation/builtins/negate.C
negate_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
negate_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

exp_la_SOURCES = computation/builtins/exp.C
exp_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
exp_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Prelude_la_SOURCES = computation/builtins/Prelude.C
Prelude_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
Prelude_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

equals_la_SOURCES = computation/builtins/equals.C
equals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
equals_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

notequals_la_SOURCES = computation/builtins/notequals.C
notequals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
notequals_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

greaterthan_la_SOURCES = computation/builtins/greaterthan.C
greaterthan_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
greaterthan_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

greaterthanorequal_la_SOURCES = computation/builtins/greaterthanorequal.C
greaterthanorequal_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
greaterthanorequal_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

lessthan_la_SOURCES = computation/builtins/lessthan.C
lessthan_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
lessthan_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

lessthanorequal_la_SOURCES = computation/builtins/lessthanorequal.C
lessthanorequal_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
lessthanorequal_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

getNucleotides_la_SOURCES = computation/builtins/getNucleotides.C
getNucleotides_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
getNucleotides_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

plus_gwF_la_SOURCES = computation/builtins/plus_gwF.C
plus_gwF_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
plus_gwF_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

alphabetSize_la_SOURCES = computation/builtins/alphabetSize.C
alphabetSize_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
alphabetSize_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

iotaUnsigned_la_SOURCES = computation/builtins/iotaUnsigned.C
iotaUnsigned_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
iotaUnsigned_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

f3x4_frequencies_la_SOURCES = computation/builtins/f3x4_frequencies.C
f3x4_frequencies_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
f3x4_frequencies_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

gtr_la_SOURCES = computation/builtins/gtr.C
gtr_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
gtr_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

m0_la_SOURCES = computation/builtins/m0.C
m0_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
m0_la_DEPENDENCIES = $(EXTRADEPENDENCIES)


set_vector_index_la_SOURCES = computation/builtins/set_vector_index.C
set_vector_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
set_vector_index_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

new_vector_la_SOURCES = computation/builtins/new_vector.C
new_vector_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
new_vector_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

get_vector_index_la_SOURCES = computation/builtins/get_vector_index.C
get_vector_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
get_vector_index_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

vector_size_la_SOURCES = computation/builtins/vector_size.C
vector_size_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
vector_size_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_fst_la_SOURCES = computation/builtins/c_fst.C
c_fst_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
c_fst_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_snd_la_SOURCES = computation/builtins/c_snd.C
c_snd_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
c_snd_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

c_pair_la_SOURCES = computation/builtins/c_pair.C
c_pair_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
c_pair_la_DEPENDENCIES = $(EXTRADEPENDENCIES)


evaluate_la_SOURCES = computation/builtins/evaluate.C
evaluate_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
evaluate_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Modifiables_la_SOURCES = computation/builtins/Modifiables.C
Modifiables_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
Modifiables_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

rs07_branch_HMM_la_SOURCES = computation/builtins/rs07_branch_HMM.C
rs07_branch_HMM_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
rs07_branch_HMM_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

rs07_lengthp_la_SOURCES = computation/builtins/rs07_lengthp.C
rs07_lengthp_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
rs07_lengthp_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

pairwise_alignment_length1_la_SOURCES = computation/builtins/pairwise_alignment_length1.C
pairwise_alignment_length1_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
pairwise_alignment_length1_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

pairwise_alignment_length2_la_SOURCES = computation/builtins/pairwise_alignment_length2.C
pairwise_alignment_length2_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
pairwise_alignment_length2_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

transition_counts_la_SOURCES = computation/builtins/transition_counts.C
transition_counts_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
transition_counts_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

pairwise_alignment_probability_from_counts_la_SOURCES = computation/builtins/pairwise_alignment_probability_from_counts.C
pairwise_alignment_probability_from_counts_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
pairwise_alignment_probability_from_counts_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

popgen_la_SOURCES = computation/builtins/popgen.C
popgen_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
popgen_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

trigger_la_SOURCES = computation/builtins/trigger.C
trigger_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
trigger_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Distribution_la_SOURCES = computation/builtins/Distribution.C
Distribution_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
Distribution_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

Range_la_SOURCES = computation/builtins/Range.C
Range_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
Range_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

SModel_la_SOURCES = computation/builtins/SModel.C
SModel_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
SModel_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

show_la_SOURCES = computation/builtins/show.C
show_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
show_la_DEPENDENCIES = $(EXTRADEPENDENCIES)

sum_out_coals_la_SOURCES = computation/builtins/sum_out_coals.C
sum_out_coals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic $(EXTRALDFLAGS)
sum_out_coals_la_DEPENDENCIES = $(EXTRADEPENDENCIES)
