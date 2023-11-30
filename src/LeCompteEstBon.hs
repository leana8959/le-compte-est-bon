module LeCompteEstBon (
  solutions,
  solution,
  solutionsFast,
  solutionFast,
)
where

import Combinations
import Control.Monad (guard)
import Expressions

import Functions

solutions :: [Int] -> Int -> [Expression]
solutions ns n = do
  choice <- choices ns
  expr <- expressions choice
  res <- evalValidExp expr
  guard (res == n)
  return expr

solution :: [Int] -> Int -> Expression
solution = head .: solutions

----------------------------------
-- Amélioration de performances --
----------------------------------

type Result = (Expression, Int)

combine :: Result -> Result -> [Result]
combine (lhs, l) (rhs, r) = do
  op <- operators
  guard (valid op l r)
  return (Apply op lhs rhs, evalExp (Apply op (Value l) (Value r)))

results :: [Int] -> [Result]
results [] = []
results [n] = [(Value n, n)]
results ns = do
  (ls, rs) <- splits ns
  resl <- results ls
  resr <- results rs
  combine resl resr

solutionsFast :: [Int] -> Int -> [Expression]
solutionsFast ns n = do
  perm <- permutations ns
  (expr, v) <- results perm
  guard (n == v)
  return expr

solutionFast :: [Int] -> Int -> Expression
solutionFast = head .: solutionsFast
