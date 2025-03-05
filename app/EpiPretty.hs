module EpiPretty where

import Data.List
import Epi
import Binop

-- Pretty print an Eπ process
parens :: String -> String
parens s = "(" <> s <> ")"

over :: String -> Epi.Process -> String
over val process =
  "overshell(" ++ prettyProcess process ++ ",\"" ++ val ++ "\")"

under :: String -> String -> String
under val log =
  "undershell(" ++ log ++ ",\"" ++ val ++ "\")"

prettyProcess :: Process -> String
prettyProcess process = case process of
  Nul -> "null"
  Send chan terms cont ->
    "send(" <> prettyChannel chan <> "," <> intercalate "\\," (map prettyTerm terms) <> ")." <> prettyProcess cont
  Recv chan vars cont ->
    "receive" <> "(" <> prettyChannel chan <> "," <> intercalate "\\," vars <> ")." <> prettyProcess cont
  Broad chan terms cont ->
    "broad(" <> prettyChannel chan <> "," <> intercalate "\\," (map prettyTerm terms) <> ")." <> prettyProcess cont
  Par p q -> prettyProcess p <> " | " <> prettyProcess q
  Rep p -> "!" <> parens (prettyProcess p)
  Res name p -> "nu " <> name <> "." <> "(" <> prettyProcess p <> ")"
  Match t1 t2 comp p q ->
    "["
      <> prettyTerm t1
      <> " "
      <> prettyComp comp
      <> " "
      <> prettyTerm t2
      <> "] ("
      <> prettyProcess p
      <> ") ("
      <> prettyProcess q
      <> ")"

-- Pretty print terms
prettyTerm :: Term -> String
prettyTerm term = case term of
  Number n -> show n
  TChan chan -> prettyChannel chan
  BinaryOp op t1 t2 ->
    prettyTerm t1 <> " " <> prettyOp op <> " " <> prettyTerm t2

-- Pretty print channels
prettyChannel :: Channel -> String
prettyChannel chan = case chan of
  Name name -> name
  Variable var -> var
  Labelled base label -> prettyChannel base <> " dot " <> label

-- Helper functions
prettyOp :: BinOp -> String
prettyOp op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"

prettyComp :: Comparison -> String
prettyComp comp = case comp of
  Eq -> "="
  Neq -> "eq.not"
  Lt -> "<"
  Gt -> ">"
  Leq -> "lt.eq"
  Geq -> "gt.eq"
