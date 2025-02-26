module Main where

import Butf

exampleExpr :: Expr
exampleExpr =
  Index (Ops (Map
    (Lambda "x" (Ops  (BinOp Add (Var "x") (Const 1))))  -- λx. x + 1
    (Array [Const 1, Const 2, Const 3]))) (Const 2)      -- [1, 2, 3]

main :: IO ()
main = do
  print (eval exampleExpr)