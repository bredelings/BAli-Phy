% Building and installing bali-phy

## Prequisites

You will need a C++ compiler that understands C++20:

* gcc 10 (or higher) works
* clang 3 (or higher) works
* XCode 14.0 (or higher) works

You will also need to install

* cairo graphics library (optional, but required to build the `draw-tree` program)

To build the executables, you will need

* meson
* ninja

To build the documentation, you will need

* pandoc

### Mac
On Macs you can install all the prerequisites using homebrew as follows:

```sh
sudo brew install meson pkg-config pandoc cairo
```

### Linux
On Debian and Ubuntu systems you can install all the prequisites with the following command:

``` sh
sudo apt-get install g++ meson pandoc libcairo2-dev
```

## Compilation

``` sh
cd BAli-Phy/
meson build --prefix=$HOME/Applications/bali-phy
ninja -C build install
```

