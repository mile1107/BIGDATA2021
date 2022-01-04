module Main where

import BoWPar
import Data.List.Split (chunksOf)

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
  let getContents = readFile "Ejemplo.txt" 
  text <- getContents
  
  let 
    tf'  = tfPar (chunksOf nchunks $ bagofwords (lines text))
    idf' = idfPar tf'
  
  print tf'
  print idf'

  return ()
