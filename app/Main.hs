module Main where

import Butf

exampleExpr :: Expr
exampleExpr =
  Index (Map (Tuple [
    (Lambda "x"  (BinOp Add (Var "x") (Const 1))), -- λx. x + 1
    (Array [Const 1, Const 2, Const 3])           -- [1, 2, 3]
  ])) (Const 2)   

main :: IO ()
main = do
  print (exampleExpr)
  print (eval exampleExpr)