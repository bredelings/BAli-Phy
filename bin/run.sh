#!/bin/sh
#$ -l serial -V -S /bin/sh -cwd

# A script for running any program on the Sun Grid Engine (SGE)

progname=$1
shift
echo working directory: $(pwd)
echo command line: "exec $progname $@"
echo job id: $JOB_ID
echo $JOB_ID > job_id
exec $progname "$@"