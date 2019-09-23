#!/bin/bash

source debian/versions.sh

sed -i "s/GHC_VERSION/$GHC_VERSION/g" debian/harbinger.install
sed -i "s/HARBINGER_VERSION/$HARBINGER_VERSION/g" debian/harbinger.install
