#!/bin/bash

CURRENT_PATH=`pwd`
MAIN=`ls *.hs`

SRC=/home/duck/programme/haskell/efa2/src


cd $SRC
ghc --make $CURRENT_PATH/$MAIN