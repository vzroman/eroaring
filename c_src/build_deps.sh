#!/bin/sh
# based on build_deps.sh from basho/eleveldb

CRoaring_VSN="v0.9.9"

set -e

# the script folder
DIR=$PWD
BASEDIR="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

# dive into c_src
cd $BASEDIR

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

case "$1" in
    clean)
        rm -rf CRoaring
        ;;

    build)
        # cJSON
        cd $BASEDIR
        if [ ! -d CRoaring ]; then
            git clone --depth 1 -b $CRoaring_VSN https://github.com/RoaringBitmap/CRoaring.git
        fi
        cd CRoaring
        mkdir -p build
        cd build
        ../amalgamation.sh
        ;;
esac

cd $DIR
