module ButfPretty where

import Butf
import Data.List

-- Pretty print an Eπ process
parens :: String -> String
parens s = "(" <> s <> ")"



prettyExpr :: Expr -> String
prettyExpr e = case e of
    Const n -> show n
    Var x -> x
    Array es -> "[" <> intercalate ", " (map prettyExpr es) <> "]"
    Tuple es -> "(" <> intercalate ", " (map prettyExpr es) <> ")"
    Index e1 e2 -> prettyExpr e1 <> "[" <> prettyExpr e2 <> "]"
    Lambda x e -> parens ("λ" <> x <> prettyExpr e)
    App e1 e2 -> prettyExpr e1 <> prettyExpr e2
    If e1 e2 e3 -> "if" <> prettyExpr e1 <> "then" <> prettyExpr e2 <> "else" <> prettyExpr e3
    Map e -> "Map" <> prettyExpr e
    Iota e -> "Iota" <> prettyExpr e
    Size e -> "Size" <> prettyExpr e
    BinOp op e1 e2 -> prettyExpr e1 <> prettyOp op <> prettyExpr e2

prettyOp :: BinOp -> String
prettyOp op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"