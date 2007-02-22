#!/bin/sh

### A script for running bali-phy on the Sun Grid Engine (SGE) ###

# % VERSION=[version] bali-phy.sh <data.fasta> [optargs]

# We assume that binaries are in ~/bin/bali-phy/$VERSION
# (or ~/bin/bali-phy/$VERSION-DEBUG)

#------------------------ qsub options: --------------------#
# -V:         Give the job access to shell environment variables
# -S /bin/sh: Use /bin/sh as the shell for the job.
# -cwd:       Execute the job from the current working directory.
# -N <name>   Specify job name

QSUB_ARGS='-V -S /bin/sh -cwd -N bali-phy'

#-------------------- Are we debugging? ---------------------#
if [ "$DEBUG" ] ; then
    DEBUG="-DEBUG";
fi

#------------------ Locate the programs ---------------------#
QSUB=$(type -p qsub)
BALI_PHY=$(type -p bali-phy${DEBUG})

if [ "$VERSION" ] ; then
    BALI_PHY=~/bin/bali-phy/${VERSION}${DEBUG}
fi

#------ Run w/o SGE if not found ----
if [ ! -x "$BALI_PHY" ] ; then
   echo "bali-phy.sh: I can't find 'bali-phy' to run it!"
   echo "bali-phy.sh: If it is installed, please add it to your $PATH"
   exit 1
fi

if [ ! -x "$QSUB" ] ; then
   echo "Not using SGE";
   ($BALI_PHY "$@" &)
   exit 0
fi

#------ start the sampler
qsub $QSUB_ARGS <<EOF 
#!/bin/sh
exec $BALI_PHY $@
EOF

