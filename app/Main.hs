module Main where

import Butf
import Data.List (intercalate)
import EpiPretty
import Epi
import Binop
import Fresh (runNameGenerator)
import Translation

main :: IO ()
main = do
  let arrayExpr = Lambda "x" (BinOp Add (Var "x") (Const 1))-- Example: [2,3][1]
      outputChannel = "o"
      generator = translateToEpi arrayExpr outputChannel
      (process, log) = runNameGenerator generator

  putStrLn "Translation Log:"
  putStrLn log
  putStrLn "\nTranslated Process:"
  print process
