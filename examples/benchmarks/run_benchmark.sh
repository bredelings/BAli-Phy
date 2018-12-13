#!/bin/bash
#
# Global variables
#
niter=50
infile="RNAPol-linsi.faa"
let min_time=180
results_file="benchmarks.tsv"
#
# Doc string
#
usage() { echo \
"Time/profile a simple fixed-time benchmark of bali-phy

Usage: $0 [-n NITER] [-f FILE] [-h] COMMAND

Where:
   	-n NITER  Number of iterations (default ${niter})
	-f FILE   FASTA file (default ${infile})
	-h        Print this message
It is recommended that NITER on FILE gives a run time of
at least $min_time seconds to get past startup effects.

Commands:
	autofdo     Profile for autofdo
	bolt        Profile for BOLT
	callgrind   Run with valgrind/callgrind
	gperf       Run with google perftools
	time LABEL  Time with label LABEL with these 
		    environmental variables set:
   			CXX
   			CXXFLAGS"  \
	1>&2; exit 1; }
check_paranoid() {
	let paranoid=$(cat /proc/sys/kernel/perf_event_paranoid)
	if test "$paranoid" -ne -1; then
		echo "Your kernel.perf_event_paranoid is set to $paranoid" 1>&2
		echo "This test cannot be run as a regular user until you" 1>&2
		echo "echo \"-1\" > /proc/sys/kernel/perf_event_paranoid" 1>&2
		exit 1
	fi
}

#
# Get required paths
#
examples_path=$(dirname "${PWD}")
install_path=$(dirname "${examples_path}")
exe_path="${install_path}/build/src/bali-phy"
package_path="${install_path}/build/src/builtins/:${install_path}/"
if test "$#" -eq 0; then
	usage
	exit 1
fi
while getopts "n:f:h" o; do
	# parse options
	case "${o}" in
		n)
			let niter=$OPTARG
			;;
		f)
			infile=$OPTARG
			;;
		h)
			usage
			exit 1
			;;
		\? )
			usage
			exit 1
			;;
	esac
done
if [ ! -e "$infile" ]; then
	echo "ERROR--File $infile does not exist" 1>&2
	exit 1
fi
infilename="${infile%.*}"
shift $((OPTIND-1))
command=$1; shift
case "$command" in
	# parse commands
	autofdo)
		check_paranoid
		echo "Running AutoFDO (CXXFLAGS needs -g)"
		perfcmd="perf record -b --"
		postprocess() {
			create_gcov --profile=perf.data --gcov=bali-phy.gcov --gcov_version=1 --binary=$exe_path 
		}
		;;
	bolt)
		check_paranoid
		echo "Running BOLT (CXXFLAGS needs -g -fno-reorder-blocks-and-partition -Wl,--emit-relocs) "
		perfcmd="perf record -e cycles:u -j any,u --"
		postprocess() {
			perf2bolt -p perf.data -o perf.fdata $exe_path
			llvm-bolt $exe_path -o ${exe_path}.bolt -data=perf.data \
				-reorder-blocks=cache+ -reorder-functions=hfsort+ \
				-split-functions=3 -split-all-cold -split-eh \
				-dyno-stats
		}
		;;
	callgrind)
		echo "Running callgrind"
		perfcmd="valgrind --tool=callgrind"
		postprocess() {
			pass
		}
		;;
	gperf)
		echo "Running gperf (CXXFLAGS needs -g -Wl,-lprofiler)"
		perfcmd=""
		export CPUPROFILE=bali-phy.gprof
		postprocess() {
			pprof --text $exe_path bali-phy.gprof > bali-phy-gperf.txt
			pprof --callgrind $exe_path bali-phy.gprof > bali-phy-gperf.callgrind
		}
		;;
	time)
		if test "$#" -ne 1; then
			echo "ERROR--time command requires a label" 1>&2
			exit 1
		fi
		label=$1
		if [ -z "${CXX}" ]; then
			echo "ERROR--\$CXX must be set before timing" 1>&2
			exit 1
		fi
		if [ -z "${CXXFLAGS}" ]; then
			echo "ERROR--\$CXXFLAGS must be set before timing" 1>&2
			exit 1
		fi
		perfcmd=""
		results_file="benchmarks.tsv"
		cpu=$(lscpu | grep -oP 'Model name:\s*\K.+')
		postprocess() {
			if [ ! -e "$results_file" ]; then # init results file
				echo -e "#Time,s\tniter\tlabel\tCPU\tCXX\tCXXFLAGS" \
					> ${results_file}
			fi
   			echo -e "${runtime}\t${niter}\t${label}\t${cpu}\t${CXX}\t${CXXFLAGS}" \
				>> ${results_file}
		}
		;;
	\? )
		echo "ERROR--Unknown command $command" 1>&2
		usage
		exit 1
		;;
esac
#
# Remove output directory so name stays the same
#
if [ -d "${infilename}-1" ]; then
	rm -rf ${infilename}-1/
fi
#
# Run the command with timing
#
let runtime=$(TIMEFORMAT='%0R'; { \
	time $perfcmd $exe_path --package-path=$package_path \
	--seed 0 -i $niter $infile \
	1>/dev/null 2>/dev/null; } 2>&1)
rm -rf ${infilename}-1/
#
# Check timing and do post-processing
#
if [ "$runtime" -lt "$min_time" ]; then
	echo "WARNING--run time of $runtime is less than recommended $min_time s"
	exit 1
else
	echo "$niter iterations on file $infile took $runtime s"
fi
postprocess
exit 0
