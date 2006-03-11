#!/bin/sh
#$ -V -S /bin/sh -cwd

# A script for running bali-phy on the Sun Grid Engine (SGE)

#### CLASS=[class] VERSION=[version] sampler.sh data.fasta [optargs]

# CLASS is optional, but VERSION is required.

# We assume that binaries are in ~/bin/bali-phy/$VERSION-NDEBUG
# (or ~/bin/bali-phy/$VERSION-DEBUG)

#---- Are we debugging? ----
if [ "$DEBUG" ] ; then
    DEBUG="-DEBUG";
fi

#---- find out which run class we are in ----
if [ ! "$CLASS" ]; then
    CLASS=normal
fi

#----- Determine the direction to run in -----
count=1
while [ -e "$CLASS-$count" ] ; do
    (( count++ ))
done

DIR="$CLASS-$count"

#------ Attempt to determine the data directory ------
if [ "${BALI_PHY_DATA-unset}" = unset ] ; then
    echo "Error: BALI_PHY_DATA is not set:"
    echo "  * set it to the location of the BAli-Phy data directory."
    echo "  * otherwise, set it to '' and specify --data-dir <dir>."
    exit 1

elif [ ! "$BALI_PHY_DATA" ] ; then
    echo "Warning: BALI_PHY_DATA is set to '':"
    echo "  * make sure you specified --data-dir <dir>."

elif [ ! -e "$BALI_PHY_DATA" ] ; then
    echo "Error: Data directory BALI_PHY_DATA='${BALI_PHY_DATA}' does not exist."
    exit 1

elif [ ! -d "$BALI_PHY_DATA" ] ; then
    echo "Error: File BALI_PHY_DATA='${BALI_PHY_DATA}' is not a directory."
    exit 1
fi

#----- Make the directory, move into it -----#
mkdir $DIR
cd $DIR

#-------- Link in the data directory --------#
ln -sf ${BALI_PHY_DATA} Data;

#----------- Determine the Version ----------#
if [ "$VERSION" ] ; then
    echo "Using specified version $VERSION."
else
    V=1
    pushd ~/bin/bali-phy > /dev/null
    for file in *${DEBUG} ; do
	V2=${file%-${DEBUG}}
	if (( V2 > V )) 2>/dev/null ; then
	    V=$V2;
	fi
    done
    VERSION=$V
    popd >/dev/null
    echo "Using highest version $VERSION."
fi

#------ start the sampler with the specified args ----
qsub=$(which qsub 2>/dev/null)
if [ -x "$qsub" ] ; then
    qsub -o out -e err sge-run.sh ~/bin/bali-phy/${VERSION}${DEBUG} "$@"
else
    (nohup sge-run.sh ~/bin/bali-phy/${VERSION}${DEBUG} "$@" >out 2>err &)
fi

