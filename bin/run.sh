#!/bin/sh
#$ -V -S /bin/sh -cwd

ulimit -t unlimited
progname=$1
shift
echo working directory: $(pwd)
echo command line: "exec $progname $@"
echo job id: $JOB_ID
echo $JOB_ID > job_id
hostname > job_node
exec $progname "$@"