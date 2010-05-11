#!/bin/bash

function warning () {
    echo "W: " $*  1>&2
}

function info () {
    echo "I: " $*  1>&2
}

function error () {
    echo "E: " $*  1>&2
}


if [ "$UID" != "0" ] ; then
    error "You must be root to run this script."
    exit 1
fi

WOOT=$(mktemp -d tmpDebootStrap_XXXXXX)
chmod og+rx ${WOOT}

set -x
debootstrap --variant=buildd --components=main,restricted,universe,multiverse karmic ${WOOT} http://mirror.anl.gov/ubuntu/ 

(cat ${WOOT}/etc/apt/sources.list | sed -e 's/^deb /deb-src /') >>${WOOT}/etc/apt/sources.list

cat <<EOF >> ${WOOT}/etc/apt/sources.list
deb file:///work/localpool karmic-seereason main
deb-src file:///work/localpool karmic-seereason main
deb http://deb.seereason.com/ubuntu karmic-seereason main
deb-src http://deb.seereason.com/ubuntu karmic-seereason main
deb http://mirror.anl.gov/ubuntu karmic-updates main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu karmic-updates main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu karmic-backports main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu karmic-backports main restricted universe multiverse
deb http://mirror.anl.gov/ubuntu karmic-security main restricted universe multiverse
deb-src http://mirror.anl.gov/ubuntu karmic-security main restricted universe multiverse
EOF

chroot ${WOOT} apt-get update


info 'Replacing ~/.autobuilder/dists/karmic-seereason/clean-Moderate'
rm -rf ~/.autobuilder/dists/karmic-seereason/clean-Moderate
mv ${WOOT} ~/.autobuilder/dists/karmic-seereason/clean-Moderate

info 'Starting autobuilder with new root'
autobuilder karmic-seereason --force haskell-devscripts --target haskell-devscripts

exit 0