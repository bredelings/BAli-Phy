#!/bin/sh
#$ -l serial -V -S /bin/sh -cwd

progname=$1
shift
echo working directory: $(pwd)
echo command line: "exec $progname $@"
echo job id: $JOB_ID
echo $JOB_ID > job_id
hostname > job_node
exec $progname "$@"