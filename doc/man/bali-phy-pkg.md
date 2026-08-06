% bali-phy-pkg(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bali-phy-pkg** - Manage BAli-Phy packages

# SYNOPSIS

**bali-phy-pkg** [command] [arguments]

# DESCRIPTION

Install and remove BAli-Phy packages.

On Linux and macOS, packages are installed in `~/.local/share/bali-phy/packages`.
On Windows, packages are installed in `%LOCALAPPDATA%\bali-phy\packages` unless `HOME` is set.

Package information is kept in the `info` directory beside the `packages` directory.

# ALL OPTIONS:

**help**, **-h**, **--help**
: Produce help message.

**available**
: List packages available on the web.

**install** _PACKAGE_
: Install package _PACKAGE_ from the web.

**install-archive** _ARCHIVE_
: Install package in local file _ARCHIVE_.

**uninstall** _PACKAGE_
: Uninstall package _PACKAGE_

**packages**
: List installed packages and their version numbers.

**info** _PACKAGE_
: Show information about the installed version of package _PACKAGE_.

**files** _PACKAGE_
: List files for installed package _PACKAGE_.

**installed**
: List files for all installed packages

**untracked**
: List files in the package directory that aren't from any installed package.

**missing**
: List installed package files that are absent from the package directory.

# REPORTING BUGS:
BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
