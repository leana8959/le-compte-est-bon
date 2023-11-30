module Main where

import LeCompteEstBon

tirage0 = [10, 1, 25, 9, 3, 6]
cible0 = 595

tirage1 = [2, 5, 6, 11]
cible1 = 77

tirage4 = [2, 5, 6, 11, 5, 10, 3, 3, 14, 7, 13]
cible4 = 6982

main :: IO ()
main = do
  print $ solution tirage4 cible4
