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
cd ${SYSROOT}

# Note that the use of gcc-posix and g++-posix means that we need
# *-posix/libgcc_s_seh-1.dll and *-posix2/libstdc++-6.dll instead
# of the *-win32/ versions.
PKGS="boost-1.89.0-1
boost-libs-1.89.0-1
eigen3-3.4.0-1
range-v3-0.12.0-1"
# nlohmann-json-3.11.2-1   This breaks things, maybe because we end up using both versions?


# We're going to use the /usr/x86_64-w64-mingw32/lib/libwinpthread-1.dll instead of downloading winpthread.

for PKG in ${PKGS} ; do
    FILE=mingw-w64-x86_64-${PKG}-any.pkg.tar.zst
    if [ -e "${FILE}" ] ; then
        echo "   ${PKG} already downloaded and installed."
    else
        URL="http://repo.msys2.org/mingw/x86_64/${FILE}"
        if ! wget --no-verbose --show-progress "${URL}" ; then
            echo "Failed to download ${URL}"
            exit
        fi
        if tar -I zstd -xf ${FILE} ; then
            echo "${PKG} installed"
        else
            rm ${FILE}
            echo "Failed to install ${PKG}"
            echo
            echo "Perhaps the decompression program zstd is not installed?"
            echo "Try 'sudo apt install zstd'."
            exit
        fi
    fi
done
fi

echo
echo "Done."
echo
