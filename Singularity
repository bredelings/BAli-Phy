BootStrap: library
From: ubuntu:22.04

%post

    export DEBIAN_FRONTEND=noninteractive

    echo "Installing required packages..."

    # bali-phy dependencies
    apt-get update -y
    apt-get install -y build-essential git cmake pandoc ccache python3-pip pkg-config
    pip install meson ninja

    apt-get install -y libboost-all-dev nlohmann-json3-dev librange-v3-dev libeigen3-dev libcairo2-dev

    # For run-time environment.
    apt-get install -y gnuplot r-base

%environment
    export PATH=$PATH:/scif/apps/bali-phy/bin

%appinstall bali-phy
    mkdir /bali-phy
    cd /bali-phy
    git clone https://github.com/bredelings/BAli-Phy.git git
    meson setup git build --prefix=${SCIF_APPROOT} --buildtype=release
    ninja -C build install

%labels
    MAINTAINER benjamin.redelings@gmail.com
