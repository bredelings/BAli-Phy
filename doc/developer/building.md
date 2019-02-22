% Building and installing bali-phy

# Building and installing bali-phy

## Prequisites

You will need a C++ compiler that understands C++17.  

* gcc 7 (or higher) works
* clang 7 (or higher) works
* XCode 10.0 (or higher) works

You will also need to install

* cairo graphics library (optional, but required to build the `draw-tree` program)

To build the executables, you will need

* meson
* ninja

To build the documentation, you will need

* pandoc

On Debian and Ubuntu systems you can install all the prequisites with the following command:

``` sh
sudo apt-get install g++ libcairo2-dev ninja-build meson pandoc
```

## Compilation

``` sh
cd BAli-Phy/
meson build --prefix=$HOME/Applications/bali-phy
cd build
ninja install
```

