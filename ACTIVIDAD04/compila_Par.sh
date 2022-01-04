#!/bin/bash

ghc -o bin/$1 --make $1.hs -threaded -eventlog -rtsopts -iBibliotecas
rm *.hi *.o
rm Bibliotecas/*.hi Bibliotecas/*.o
