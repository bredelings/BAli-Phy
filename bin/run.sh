#!/bin/sh
#$ -V -S /bin/sh -cwd

# don't quit after 20 minutes! :P
ulimit -t unlimited

# get the arguments
progname=$1
shift

# determine the job file name -----
JOB_FILE=job_info
if [ -e "$JOB_FILE" ] ; then
    count=1
    while [ -e job_info.$count ] ; do
	(( count++ ))
    done

    JOB_FILE=job_info.$count
fi

# echo information to job_info file
echo job id: $JOB_ID
{  
  echo "command: $progname $@"
  echo "directory: $(pwd)"
  if [ "$JOB_ID" ] ; then 
    echo "job id: $JOB_ID"
  fi
  echo "hostname: $(hostname)"
  echo "PID: $$"
} > $JOB_FILE

# echo information to beginning of STDOUT
echo working directory: $(pwd)
echo command: "$progname $@"

# exec the program
exec $progname "$@"