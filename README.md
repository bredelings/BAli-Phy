![Build and test](https://github.com/bredelings/BAli-Phy/workflows/Build%20and%20test/badge.svg)

Install
-------

If you just want to install bali-phy, please visit the [release page](http://www.bali-phy.org/download.php).  If you want to compile BAli-phy from source, the quick start instructions are below.

Compiling
---------

You will need a C++ compiler that understands C++17.
 * gcc 9 (or higher) works
 * clang 8 (or higher) works
 * XCode 11 (or higher) works

Install Prerequisites
---------------------
On Ubuntu, you can use apt-get:
```bash
sudo apt-get install g++ libcairo2-dev meson
```

On Mac (or Linux, actually) you can use homebrew:
```bash
brew install cairo meson
```

If the version of meson is not at least 0.56, then you need to install
meson through the python package manager "pip" or "pip3":

    pip3 -V
    PATH=$HOME/.local/bin:$PATH
    pip3 install --user meson ninja


Build BAli-Phy
--------------
```
git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy
meson build --prefix=$HOME/Applications/bali-phy
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


Further Documentation
---------------------

* [http://bali-phy.org/](http://bali-phy.org/)
* [Manual](http://bali-phy.org/README.xhtml)
* [Tutorial](http://bali-phy.org/Tutorial4.html)

The Manual describes [how to install](http://bali-phy.org/README.xhtml#installation) bali-phy in greater detail.

