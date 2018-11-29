% bali-phy-pkg(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bali-phy-pkg** - Manage BAli-Phy packages

# SYNOPSIS

**bali-phy-pkg** [command] [arguments]

# DESCRIPTION

Install and remove BAli-Phy packages.

Packages are installed in `~/.local/share/bali-phy/packages`.

Package information is kept in `~/.local/share/bali-phy/info/`.

# ALL OPTIONS:

**-h**, **--help**, **help**
: Produce help message.

**install** _PACKAGE_
: Install package _PACKAGE_ from the web.

**install-archive** _ARCHIVE_
: Install package in local file _ARCHIVE_.

**available**
: List packages available on the web.

**uninstall** _PACKAGE_
: Uninstall package _PACKAGE_

**info** _PACKAGE_
: Show information about the installed version of package _PACKAGE_.

**packages**
: List installed packages and their version numbers.

**files** _PACKAGE_
: List files for installed package _PACKAGE_.

**installed**
: List files for all installed packages

**untracked** _PACKAGE_
: List files in the package directory that aren't from any installed package.

# REPORTING BUGS:
BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

