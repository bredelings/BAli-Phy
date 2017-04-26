Documentation
------------

* [http://bali-phy.org/](http://bali-phy.org/)
* [Manual](http://bali-phy.org/README.xhtml)
* [Tutorial](http://bali-phy.org/Tutorial2.html)

Compiling
---------

You will need a C++ compiler that understands C++14.  These include
 * gcc 5 (or higher)
 * clang 3.9 (or higher)

Build
-----

```bash
git clone https://github.com/bredelings/BAli-Phy.git
mkdir bali-phy-<version>-build
cd bali-phy-<version>-build
../bali-phy/configure --prefix=$HOME/local/
make
make install
```

If you don't specify a prefix, the default is to install files under `/usr/local`.

Extra options to `configure` can be revealed by supplying the `--help` flag.

Installed locations
------------------
If you installed in $HOME/local as recommended above, then files will be in:

|----Location---------------------|-----------Files---------|
| ~/local/bin                     | Binary executables.     |
| ~/local/share/bali-phy/examples | Example files.          |
| ~/local/share/bali-phy/Data     | Data directory.         |
| ~/local/share/doc/bali-phy/     | Documentation.          |



