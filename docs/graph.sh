#!/usr/bin/sh
cabal run docs -- graph -o docs.dot
cat docs.dot | dot -Tsvg | kitty +kitten icat
