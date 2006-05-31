#!/bin/sh

#### A script for running bali-phy on the Sun Grid Engine (SGE)

# % VERSION=[version] bali-phy.sh <data.fasta> [optargs]

# We assume that binaries are in ~/bin/bali-phy/$VERSION-NDEBUG
# (or ~/bin/bali-phy/$VERSION-DEBUG)

#---- Are we debugging? ----
if [ "$DEBUG" ] ; then
    DEBUG="-DEBUG";
fi

#----------- Determine the Version ----------#
if [ "$VERSION" ] ; then
    BALI_PHY=~/bin/bali-phy/${VERSION}${DEBUG}
else
    BALI_PHY=$(type -p bali-phy${DEBUG})
fi

QSUB=$(type -p qsub)
#------ Run w/o SGE if not found ----
if [ ! -x "$QSUB" ] ; then
   echo "Not using SGE";
   ($BALI_PHY "$@" &)
   exit 0
fi

if [ ! -x "$BALI_PHY" ] ; then
   echo "bali-phy-sge: I can't find 'bali-phy' to run it!"
   echo "bali-phy-sge: If it is installed, please add it to your $PATH"
   exit 1
fi

#------ write the script
RUN_SH=/tmp/bali-phy-sge-$$
trap "rm -f $RUN_SH" 2 3 15

echo >$RUN_SH '#!/bin/sh
exec $@'

#------ start the sampler

QSUB_ARGS='-V -S /bin/sh -cwd'

qsub $QSUB_ARGS $RUN_SH $BALI_PHY $@

#------ clean up 
rm -f $RUN_SH
