#!/bin/bash

# parse command line arguments
while echo $1 | grep ^- > /dev/null; do
    # intercept help while parsing "-key value" pairs
    if [ "$1" = "--help" ] || [ "$1" = "-h" ]
    then
        echo 'Command line options are:
-h                              : print this help and exit.
-download        <true|false>   : download MINGW packages.

WINROOT overrides the default sysroot directory ($HOME/win_root_ucrt64).

Example:
  ./make_winroot.sh -help true'
        exit
    fi

    # parse pairs
    eval $( echo $1 | sed 's/-//g' | tr -d '\012')=$2
    shift
    shift
done

# Use a separate tree so an existing MSVCRT sysroot is never upgraded in place.
SYSROOT=${WINROOT:-$HOME/win_root_ucrt64}

# 1. Make sysroot
echo
echo "1. Writing sysroot dir ${SYSROOT}"
mkdir -p "${SYSROOT}"
mkdir -p "${SYSROOT}/bin"

# 2. Download packages
if [ "$download" = "false" ] ; then
    echo
    echo "(2.) Skipping installation of packages to ${SYSROOT}."
    echo
else

    echo
    echo "2. Installing packages to ${SYSROOT}"
    echo
PACMAN_DB="${SYSROOT}/var/lib/pacman"
if [ -d "${SYSROOT}/ucrt64" ] && [ ! -d "${PACMAN_DB}/local" ] ; then
    echo "The existing sysroot predates Pacman and cannot be upgraded safely."
    echo "Move or remove ${SYSROOT}, then run this script again."
    exit 1
fi

PACMAN_CONFIG="${SYSROOT}/pacman-cross.conf"
EMPTY_HOOKS="${SYSROOT}/var/empty-pacman-hooks"
mkdir -p "${PACMAN_DB}" "${SYSROOT}/var/cache/pacman/pkg" "${SYSROOT}/var/log" "${EMPTY_HOOKS}"

cat > "${PACMAN_CONFIG}" <<EOF
[options]
Architecture = x86_64
SigLevel = Never

[ucrt64]
Server = https://repo.msys2.org/mingw/ucrt64
EOF

# Target-side scriptlets and hooks cannot run in the Linux cross sysroot and are not needed for linking.
PACMAN=(fakeroot pacman --config "${PACMAN_CONFIG}" --root "${SYSROOT}"
        --hookdir "${EMPTY_HOOKS}" --noscriptlet --noconfirm)
if ! "${PACMAN[@]}" -Sy ; then
    echo "Failed to synchronize the MSYS2 package database."
    exit 1
fi
if ! "${PACMAN[@]}" -S --needed \
        mingw-w64-ucrt-x86_64-gcc \
        mingw-w64-ucrt-x86_64-boost \
        mingw-w64-ucrt-x86_64-cairo \
        mingw-w64-ucrt-x86_64-cli11 \
        mingw-w64-ucrt-x86_64-eigen3 \
        mingw-w64-ucrt-x86_64-range-v3 ; then
    echo "Failed to install MSYS2 packages into ${SYSROOT}."
    exit 1
fi
fi

# 3. Generate cross file
CROSSNAME=win64-cross.txt
echo
echo "3. Writing cross file to '${CROSSNAME}'"

# The GCC package supplies development files as well as Windows compiler executables.
# We use its development files with the Linux compiler, never its Windows executables.
CXX_HEADERS=("${SYSROOT}"/ucrt64/include/c++/*)
if [ "${#CXX_HEADERS[@]}" -ne 1 ] || [ ! -f "${CXX_HEADERS[0]}/vector" ]; then
    echo "Expected one MSYS2 C++ header directory; rerun with -download true." >&2
    exit 1
fi

cat > "${CROSSNAME}" <<EOF
[binaries]
c = ['ccache','x86_64-w64-mingw32ucrt-gcc']
cpp = ['ccache','x86_64-w64-mingw32ucrt-g++']
ar = 'x86_64-w64-mingw32ucrt-ar'
strip = 'x86_64-w64-mingw32ucrt-strip'
dlltool = 'x86_64-w64-mingw32ucrt-dlltool'
pkg-config = 'pkg-config'
exe_wrapper = 'wine' # A command used to run generated executables.

# Use one MSYS2 set of headers, startup objects, import libraries, and runtime DLLs.
# In particular, its __cxa_thread_atexit lives in the static MinGW support library,
# whereas Debian's import library expects that symbol in libstdc++-6.dll.
[built-in options]
c_args = ['-isystem', '${SYSROOT}/ucrt64/include']
c_link_args = ['-B${SYSROOT}/ucrt64/lib/', '-L${SYSROOT}/ucrt64/lib']

cpp_args = ['-nostdinc++',
            '-isystem', '${CXX_HEADERS[0]}',
            '-isystem', '${CXX_HEADERS[0]}/x86_64-w64-mingw32',
            '-isystem', '${CXX_HEADERS[0]}/backward',
            '-isystem', '${SYSROOT}/ucrt64/include']
cpp_link_args = ['-B${SYSROOT}/ucrt64/lib/', '-L${SYSROOT}/ucrt64/lib']

[properties]
sys_root = '${SYSROOT}'
pkg_config_libdir = '${SYSROOT}/ucrt64/lib/pkgconfig'
boost_root='${SYSROOT}/ucrt64'
mingw_prefix='${SYSROOT}/ucrt64'

[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'
EOF

echo
echo "Done."
echo
