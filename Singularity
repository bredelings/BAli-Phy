BootStrap: docker
From: ubuntu:24.04

%post

    export DEBIAN_FRONTEND=noninteractive

    echo "Installing required packages..."

    # bali-phy dependencies
    apt-get update -y
    apt-get install -y build-essential git cmake pandoc ccache python3-full pkg-config
    python3 -m venv /venv
    /venv/bin/pip install meson ninja

    apt-get install -y libboost-all-dev nlohmann-json3-dev librange-v3-dev libeigen3-dev libcairo2-dev

    # For run-time environment.
    apt-get install -y gnuplot r-base

%environment
    export PATH=$PATH:/scif/apps/bali-phy/bin

%appinstall bali-phy
    export PATH=$PATH:/venv/bin
    mkdir /bali-phy
    cd /bali-phy
    git clone https://github.com/bredelings/BAli-Phy.git git
    ( cd git; git checkout {{ version }} )
    meson setup git build --prefix=${SCIF_APPROOT} --buildtype=release
    ninja -C build install

%labels
    MAINTAINER benjamin.redelings@gmail.com
