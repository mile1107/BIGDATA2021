#!/bin/bash

ghc -o bin/$1 --make $1.hs -iBibliotecas
rm *.hi *.o
rm Bibliotecas/*.hi Bibliotecas/*.o
