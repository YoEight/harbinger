#!/bin/bash

source debian/versions.sh

sudo apt-get update
sudo apt-get install -y \
        software-properties-common \
        libz-dev

sudo add-apt-repository -y ppa:hvr/ghc

sudo apt-get install -y \
        haskell-devscripts \
        devscripts \
        pkg-haskell-tools

sudo apt-get install -y \
        ghc-$GHC_VERSION \
        ghc-$GHC_VERSION-dyn \
        ghc-$GHC_VERSION_doc \
	      ghc-$GHC_VERSION_prof \
        cabal-install-$CABAL_VERSION

