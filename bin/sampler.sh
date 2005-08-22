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
else
    DEBUG="-NDEBUG";
fi

#---- find out which run class we are in ----
if [ "$CLASS" ]; then
    true;
else
    CLASS=normal
fi

#----- Determine the direction to run in -----
count=1
while [ -e $"$CLASS-$count" ] ; do
    (( count++ ))
done

DIR="$CLASS-$count"

#---- Make the directory and move into it ----
mkdir $DIR
cd $DIR

#------ Link in the data directory ------
if [ ! -e Data ] ; then
    ln -sf ~/Devel/Sampler/Data
fi

#--------- Determine the Version --------#
if [ "$VERSION" ] ; then
    echo "Using specified version $VERSION."
else
    V=1
    pushd ~/bin/bali-phy > /dev/null
    for file in *-NDEBUG ; do
	V2=${file%-NDEBUG}
	if (( V2 > V )) 2>/dev/null ; then
	    V=$V2;
	fi
    done
    VERSION=$V
    popd >/dev/null
    echo "Using highest version $VERSION."
fi
exit
#------ start the sampler with the specified args ----
qsub=$(which qsub)
if [ "$qsub" ] ; then
    qsub -o out -e err ~/bin/run.sh ~/bin/bali-phy/${VERSION}${DEBUG} "$@"
else
    (nohup ~/bin/run.sh ~/bin/bali-phy/${VERSION}${DEBUG} "$@" >out 2>err &)
fi
