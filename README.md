![Build and test](https://github.com/bredelings/BAli-Phy/workflows/Build%20and%20test/badge.svg)
[![Appveyor](https://ci.appveyor.com/api/projects/status/q68hnnoelqqvwsy2?svg=true)](https://ci.appveyor.com/project/bredelings/bali-phy)

Install
-------

Please visit the [releases page](http://www.bali-phy.org/download.php) to download official binaries.

You can also install via homebrew on a Mac, and using `apt-get` on recent version of Debian or Ubuntu.

Documentation
------------

* [http://bali-phy.org/](http://bali-phy.org/)
* [Manual](http://bali-phy.org/README.xhtml)
* [Tutorial](http://bali-phy.org/Tutorial3.html)
* [Developer's Guide](http://bali-phy.org/developer.html)

The Manual describes [how to install](http://bali-phy.org/README.xhtml#installation) bali-phy in detail.  Simplified instructions are below.

Compiling
---------

You will need a C++ compiler that understands C++17.
 * gcc 9 (or higher) works
 * clang 8 (or higher) works
 * XCode 11 (or higher) works

You will also need to install
 * cairo graphics library (optional, but required to build the `draw-tree` program)

You will also need
 * python3
 * ninja
 * meson >= 0.53

Install Prerequisites
---------------------
```bash
sudo apt-get install g++ libcairo2-dev ninja-build python3
```
You also need to install meson.  First try:
```bash
sudo apt-get install meson
```

If the version of meson is not at least 0.49, then you need to install
meson through pip:
```bash
python3 -m venv meson
source meson/bin/activate
pip3 install meson
```

Build BAli-Phy
--------------
```
git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy
meson build --prefix=$HOME/Applications/bali-phy --buildtype=release
ninja -C build install
ninja -C build test
```

Adding bali-phy to your `$PATH`
------------------------------

In order to run the installed software, you should [add bali-phy to your $PATH](http://bali-phy.org/README.xhtml#path).

Installed locations
------------------

If you installed in `$HOME/Applications/bali-phy/` as recommended above, then files will be in:

| Location                                                       | Files                   |
| -------------------------------------------------------------- | ----------------------- |
| ~/Applications/bali-phy/bin                                    | Binary executables.     |
| ~/Applications/bali-phy/share/bali-phy/examples/sequences      | Example files.          |
| ~/Applications/bali-phy/share/doc/bali-phy/                    | Documentation.          |


