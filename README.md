![Build and test](https://github.com/bredelings/BAli-Phy/workflows/Build%20and%20test/badge.svg)

Install
-------

If you just want to install bali-phy, please visit the [release page](http://www.bali-phy.org/download.php).  If you want to compile BAli-phy from source, the quick start instructions are below.

Compiling
---------

You will need a C++ compiler that understands C++23.
 * gcc 12 (or higher) works
 * clang 18 (or higher) works
 * XCode 15 (or higher) works

Install Prerequisites
---------------------
On Ubuntu 24.04 (or higher), or on Debian testing, you can use apt-get:
```bash
sudo apt-get install g++ libcairo2-dev meson libboost-all-dev
```

On Mac (or Linux, actually) you can use homebrew:
```bash
brew install cairo meson boost
```

On miniconda, you can use:
```
conda create -n devel -c conda-forge --strict-channel-priority
conda activate devel
conda install meson gxx boost-cpp cmake pkg-config cairo
export BOOST_ROOT=$CONDA_PREFIX
```

### Meson version

After installing prerequisites above, check that the meson version is at least 1.1.

    meson --version

If your version of meson is less than 1.1, you need to install the most recent version of meson into a python virtual environment:

    python3 -m venv meson_venv
    source meson_venv/bin/activate
    pip install meson ninja


Build BAli-Phy
--------------

This will build the latest unreleased 4.0-beta version of BAli-Phy, which fixes some memory issues in 3.6.  There are some changes to the model language.  Check the NEWS file for the details.

```
git clone https://github.com/bredelings/BAli-Phy.git
cd BAli-Phy
meson setup build --prefix=$HOME/Applications/bali-phy
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

