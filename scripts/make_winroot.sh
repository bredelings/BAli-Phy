#!/bin/bash

# parse command line arguments
while echo $1 | grep ^- > /dev/null; do
    # intercept help while parsing "-key value" pairs
    if [ "$1" = "--help" ] || [ "$1" = "-h" ]
    then
        echo 'Command line options are:
-h                              : print this help and exit.
-download        <true|false>   : download MINGW packages.

Example:
  ./make_winroot.sh -help true'
        exit
    fi

    # parse pairs
    eval $( echo $1 | sed 's/-//g' | tr -d '\012')=$2
    shift
    shift
done

SYSROOT=$HOME/win_root

# 1. Make sysroot
echo
echo "1. Writing sysroot dir ${SYSROOT}"
mkdir -p "${SYSROOT}"
mkdir -p "${SYSROOT}/bin"

# 2. Generate cross file
CROSSNAME=win64-cross.txt
echo
echo "2. Writing cross file to '${CROSSNAME}'"

cat > "${CROSSNAME}" <<EOF
[binaries]
c = ['ccache','x86_64-w64-mingw32-gcc-posix']
cpp = ['ccache','x86_64-w64-mingw32-g++-posix']
ar = 'x86_64-w64-mingw32-ar'
strip = 'x86_64-w64-mingw32-strip'
dlltool = 'x86_64-w64-mingw32-dlltool'
pkg-config = 'pkg-config'
exe_wrapper = 'wine' # A command used to run generated executables.

# why do we still need these? shouldn't they get added automatically if we find boost?
[built-in options]
c_args = ['-I${SYSROOT}/mingw64/include']
c_link_args = ['-L${SYSROOT}/mingw64/lib']

cpp_args = ['-I${SYSROOT}/mingw64/include']
cpp_link_args = ['-L${SYSROOT}/mingw64/lib']

[properties]
sys_root = '${SYSROOT}'
pkg_config_libdir = '${SYSROOT}/mingw64/lib/pkgconfig'
boost_root='${SYSROOT}/mingw64'
mingw_prefix='${SYSROOT}/mingw64'

[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'
EOF

# 3. Download packages
if [ "$download" = "false" ] ; then
    echo
    echo "(3.) Skipping installation of packages to ${SYSROOT}."
    echo
else

    echo
    echo "3. Installing packages to ${SYSROOT}"
    echo
PACMAN_DB="${SYSROOT}/var/lib/pacman"
if [ -d "${SYSROOT}/mingw64" ] && [ ! -d "${PACMAN_DB}/local" ] ; then
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

[mingw64]
Server = https://repo.msys2.org/mingw/mingw64
EOF

# Target-side scriptlets and hooks cannot run in the Linux cross sysroot and are not needed for linking.
PACMAN=(fakeroot pacman --config "${PACMAN_CONFIG}" --root "${SYSROOT}"
        --hookdir "${EMPTY_HOOKS}" --noscriptlet --noconfirm)
if ! "${PACMAN[@]}" -Sy ; then
    echo "Failed to synchronize the MSYS2 package database."
    exit 1
fi
if ! "${PACMAN[@]}" -S --needed \
        mingw-w64-x86_64-boost \
        mingw-w64-x86_64-cairo \
        mingw-w64-x86_64-eigen3 \
        mingw-w64-x86_64-range-v3 ; then
    echo "Failed to install MSYS2 packages into ${SYSROOT}."
    exit 1
fi
fi

echo
echo "Done."
echo
