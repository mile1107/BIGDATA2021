module Main where

import BoW

-- |programa principal
main :: IO ()
main = do
  let getContents = readFile "Ejemplo.txt" 
  text <- getContents
  
  let 
    tf'  = tf (bagofwords (lines text))
    idf' = idf tf'
    ng'  = tf (getNgrams 2 (lines text))
    sk'  = tf (getSkipgrams 2 3 (lines text))
  
  print tf'
  print ng'
  print sk'
  print (tfidf tf' idf')
  
  return ()
