module Main where

import Binop
import Butf
import Data.List (intercalate)
import Epi
import EpiPretty
import Fresh (runNameGenerator)
import Translation

main :: IO ()
main = do
  let arrayExpr = Map (Tuple [Lambda "x" (Var "x"), Array [Const 1]]) -- Example: [2,3][1]
      outputChannel = "o"
      generator = translateToEpi arrayExpr outputChannel
      (process, log) = runNameGenerator generator

  putStrLn "Translation Log:"
  putStrLn log
  putStrLn "\nTranslated Process:"
  print process
  putStrLn "\nEvaluated Process:"
  print (eval arrayExpr)