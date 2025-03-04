module Main where

import Butf
import Epi
import Translation
import Fresh
import EpiPretty
import ButfPretty
exampleExpr :: Expr
exampleExpr =
  Index (Map (Tuple [
    Lambda "x"  (BinOp Butf.Add (Var "x") (Const 1)),  -- λx. x + 1
    Array [Const 1, Const 2, Const 3]             -- [1, 2, 3]
  ])) (Const 2)

main :: IO ()
main = do
  -- Run the generator to get the translated process
  let ex1 = Array [Const 2, Const 3]
  let ex2 = Index (Array [Const 2, Const 3]) (Const 1)
  let tp1 = runNameGenerator (translateToEpi ex1 "o")
  let tp2 = runNameGenerator (translateToEpi ex2 "o")
  putStrLn ("Butf: "<> prettyExpr ex1)
  putStrLn ("E𝜋: $"<>prettyProcess tp2 <> "$")
  --putStrLn ("E𝜋: $"<>prettyProcess translatedProcess2 <> "$")