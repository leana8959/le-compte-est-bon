module Combinations where

import Control.Arrow (Arrow (first))
import Control.Monad ((<=<))

splits :: [a] -> [([a], [a])]
splits []       = []
splits [_]      = []
splits (x : xs) = ([x], xs) : (first (x :) <$> splits xs)

subLists :: [a] -> [[a]]
subLists []       = [[]]
subLists [x]      = [[], [x]]
subLists (x : xs) = subLists xs ++ ((x :) <$> subLists xs)

inserts :: a -> [a] -> [[a]]
inserts y l = case l of
  []       -> [[y]]
  (x : xs) -> (y : l) : ((x :) <$> inserts y xs)

permutations :: [a] -> [[a]]
permutations []       = [[]]
permutations [x]      = [[x]]
permutations (x : xs) = permutations xs >>= inserts x

choices :: [a] -> [[a]]
choices = permutations <=< subLists
