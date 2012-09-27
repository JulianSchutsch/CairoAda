#!/bin/sh

# Simple script that can be used to generate Ada doc using gnathtml.
# Must be called after src files have been compiled

DOCDIR=`pwd`/`dirname $0`
rm -fr $DOCDIR/html
cd $DOCDIR/../src/cairo

files=`ls cairo*`
gnathtml.pl -I../../obj/cairo $files -ext html -o$DOCDIR/html
