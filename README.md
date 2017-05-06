[![Build Status](https://www.travis-ci.org/bredelings/BAli-Phy.svg?branch=master
)](https://www.travis-ci.org/bredelings/BAli-Phy)

Install
-------

Please visit the [releases page](http://www.bali-phy.org/download.php) to download official binaries.

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
 * XCode 7.3 (or higher) works

Build
-----

```bash
git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy/
./bootstrap.sh
cd ..
mkdir bali-phy-build
cd bali-phy-build
../BAli-Phy/configure --prefix=$HOME/Applications/bali-phy/
make
make install
```

Extra options to `configure` can be revealed by supplying the `--help` flag.

Adding bali-phy to your `$PATH`
------------------------------

In order to run the installed software, you should [add bali-phy to your $PATH](http://bali-phy.org/README.xhtml#installation).

Installed locations
------------------

If you installed in $HOME/local as recommended above, then files will be in:

| Location                                                       | Files                   |
| -------------------------------------------------------------- | ----------------------- |
| ~/Applications/bali-phy/bin                                    | Binary executables.     |
| ~/Applications/bali-phy/share/bali-phy/examples/sequences      | Example files.          |
| ~/Applications/bali-phy/share/doc/bali-phy/                    | Documentation.          |


