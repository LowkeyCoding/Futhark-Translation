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
    (Lambda "x"  (BinOp Butf.Add (Var "x") (Const 1))),  -- λx. x + 1
    (Array [Const 1, Const 2, Const 3])             -- [1, 2, 3]
  ])) (Const 2)   

main :: IO ()
main = do
  -- Run the generator to get the translated process
  let example = (Tuple [Const 1, Const 2])
  let translatedProcess = runNameGenerator (translateToEpi example "o")
  putStrLn ("Butf: "<>(prettyExpr example))
  putStrLn ("E𝜋: $"<>(prettyProcess translatedProcess) <> "$")