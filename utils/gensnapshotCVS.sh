#!/bin/sh

# create a snapshot using files on the CVS server

cairoversion=1.8
releasedate=`date "+%Y%m%d"`
workdir=/tmp/$releasedate

# package base name
pbname=cairoada-$cairoversion-$releasedate

# Make sure the target dir exists and is empty
rm -fr $workdir
mkdir -p $workdir
cd $workdir

# checkout data from server
cvs -d:pserver:anonymous:anonymous@cairoada.cvs.sourceforge.net:/cvsroot/cairoada login
cvs -z3 -d:pserver:anonymous@cairoada.cvs.sourceforge.net:/cvsroot/cairoada co -P cairoada
cvs -d:pserver:anonymous@cairoada.cvs.sourceforge.net:/cvsroot/cairoada logout

# rename project root dir
mv cairoada $pbname
# remove undesired files/dirs
rm -fr `find . -name CVS`
# create the archive
tar cvzf $pbname.tgz $pbname
echo "$workdir/$pbname.tgz has been created"
