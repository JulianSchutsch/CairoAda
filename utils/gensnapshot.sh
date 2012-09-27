#!/bin/sh

projectroot=`pwd`/`dirname $0`/..

cairoversion=1.8
releasedate=`date "+%Y%m%d"`
tmpdir=/tmp

# package base name
pbname=cairoada-$cairoversion-$releasedate
targetdir=/tmp/$pbname

echo $pbname

# Make sure the target dir exists and is empty
rm -fr $targetdir
mkdir -p $targetdir
# Remove previously created archives
#rm -f /tmp/$pbname.tgz
# Copy everything in the target dir
cp -Rp $projectroot/* $targetdir

# Remove unwanted files/dirs from the copied ones
cd $targetdir
make clean
rmdir lib obj
# Notably, remove CVS files
rm -fr `find . -name CVS`

# Make the archive(s)
cd ..
tar cvzf $pbname.tgz $pbname
# Clean
#rm -fr $pbname

echo
echo "/tmp/$pbname.tgz has been created"

