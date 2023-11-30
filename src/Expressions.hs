module Expressions where

import Prelude hiding (fst, snd)

import Combinations
import Control.Monad (guard)
import Functions

data Operator
  = Add
  | Sub
  | Mul
  | Div

instance Show Operator where
  show = fst . semantics

type Validity = Int -> Int -> Bool
type Operation = Int -> Int -> Int

validityAdd, validitySub, validityMul, validityDiv :: Validity
validityAdd = const (const True)
validitySub = const (const True)
validityMul = const (const True)
validityDiv = const (0 /=)

semantics :: Operator -> (String, Operation, Validity)
semantics Add = ("+", (+), validityAdd)
semantics Sub = ("-", (-), validitySub)
semantics Mul = ("×", (*), validityMul)
semantics Div = ("÷", div, validityDiv)

operators :: [Operator]
operators = [Add, Sub, Mul, Div]

evalOp :: Operator -> Int -> Int -> Int
evalOp = snd . semantics

valid :: Operator -> Int -> Int -> Bool
valid = trd . semantics

data Expression
  = Value Int
  | Apply Operator Expression Expression

instance Show Expression where
  show (Value v) = show v
  show (Apply op lhs rhs) =
    concat ["(", show lhs, show op, show rhs, ")"]

expressions :: [Int] -> [Expression]
expressions [] = []
expressions [n] = [Value n]
expressions ns = do
  (ls, rs) <- splits ns
  lexp <- expressions ls
  rexp <- expressions rs
  op <- operators
  return $ Apply op lexp rexp

evalExp :: Expression -> Int
evalExp (Value v) = v
evalExp (Apply op lhs rhs) = evalOp op (evalExp lhs) (evalExp rhs)

evalValidExp :: Expression -> [Int]
evalValidExp (Value v) = [v]
evalValidExp (Apply op lhs rhs) = do
  l <- evalValidExp lhs
  r <- evalValidExp rhs
  guard (valid op l r)
  return $ evalExp (Apply op lhs rhs)
