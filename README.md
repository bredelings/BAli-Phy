[![Build Status](https://www.travis-ci.org/bredelings/BAli-Phy.svg?branch=master
)](https://www.travis-ci.org/bredelings/BAli-Phy)

Install
-------

Please visit the [releases page](http://www.bali-phy.org/download.php) to download official binaries.

You can also install via homebrew on a Mac.

Documentation
------------

* [http://bali-phy.org/](http://bali-phy.org/)
* [Manual](http://bali-phy.org/README.xhtml)
* [Tutorial](http://bali-phy.org/Tutorial2.html)

The Manual describes [how to install](http://bali-phy.org/README.xhtml#installation) bali-phy in detail.  Simplified instructions are below.

Compiling
---------

You will need a C++ compiler that understands C++14.  
 * gcc 5 (or higher) works
 * clang 3.5 (or higher) works
 * XCode 6.0 (or higher) works

You will also need to install
 * cairo graphics library (optional, but required to build the `draw-tree` program)

If you build with meson and ninja, you need
 * python3
 * ninja

If you build with autotools, you need
 * autoconf
 * automake
 * libtool

Build with meson (fastest)
----------------
```bash
sudo apt-get install g++ libcairo2-dev ninja-build python3

git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy/
git submodule update --init

# We need a recent version of meson
python3 -m venv meson
source meson/bin/activate
pip3 install meson

meson build --prefix=$HOME/Applications/bali-phy  # Two warnings about 'export_dynamic' are OK.
cd build
ninja install
```

Build with autotools (slower)
-------------------

```bash
sudo apt-get install g++ libcairo2-dev autoconf automake libtool

git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy/
git submodule update --init

./bootstrap.sh
mkdir build
cd build
../configure --prefix=$HOME/Applications/bali-phy/
make
make check
make install
```

Additional options to `configure` can be revealed by supplying the `--help` flag.

Adding bali-phy to your `$PATH`
------------------------------

In order to run the installed software, you should [add bali-phy to your $PATH](http://bali-phy.org/README.xhtml#installation).

Installed locations
------------------

If you installed in `$HOME/Applications/bali-phy/` as recommended above, then files will be in:

| Location                                                       | Files                   |
| -------------------------------------------------------------- | ----------------------- |
| ~/Applications/bali-phy/bin                                    | Binary executables.     |
| ~/Applications/bali-phy/share/bali-phy/examples/sequences      | Example files.          |
| ~/Applications/bali-phy/share/doc/bali-phy/                    | Documentation.          |


