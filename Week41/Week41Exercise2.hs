module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr a
  = Var a
  | Lit Integer
  | Mul (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Eq, Show)

eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value
eval (Lit n) _ = Just (fromInteger n)
eval (Var v) m = Map.lookup v m
eval (Add e1 e2) m = do
  v1 <- eval e1 m
  v2 <- eval e2 m
  return (v1 + v2)
eval (Mul e1 e2) m = do
  v1 <- eval e1 m
  v2 <- eval e2 m
  return (v1 * v2)