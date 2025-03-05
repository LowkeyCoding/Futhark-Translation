module Butf where
import Binop
-- AST for ButF expressions
data Expr
  = Const Int
  | Var String
  | Array [Expr]
  | Tuple [Expr]
  | Index Expr Expr
  | Lambda String Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Map Expr
  | Iota Expr
  | Size Expr
  | BinOp BinOp Expr Expr
  deriving (Show, Eq)

-- Evaluation function
eval :: Expr -> Expr
eval (Const n) = Const n
eval (Var x) = error $ "Unbound variable: " ++ x
eval (Array es) = Array $ map eval es
eval (Tuple es) = Tuple $ map eval es
eval (Index arr idx) = case (eval arr, eval idx) of
  (Array arr', Const i) -> arr' !! i
  _ -> error "Indexing error"
eval (Lambda x body) = Lambda x body
eval (App (Lambda x body) arg) = eval $ substitute x (eval arg) body
eval (App f arg) = eval $ App (eval f) arg
eval (If cond thenExpr elseExpr) = case eval cond of
  Const 0 -> eval elseExpr
  _ -> eval thenExpr
eval (BinOp op e1 e2) = case (eval e1, eval e2) of
  (Const n1, Const n2) -> Const $ applyOp op n1 n2
  _ -> error "Binary operation error"
eval (Map e) = case (eval e) of
  Tuple [Lambda x body, Array arr] -> Array $ map (eval . App (Lambda x body)) arr
  res -> error ("Map error: expected a tuple (function, array) got:" ++ (show res))
eval (Iota n) = case eval n of
  Const n' -> Array $ map Const [0 .. n' - 1]
  _ -> error "Iota error"
eval (Size arr) = case eval arr of
  Array arr' -> Const $ length arr'
  _ -> error "Size error"

-- Substitute a variable with an expression in another expression
substitute :: String -> Expr -> Expr -> Expr
substitute x val (Var y) = if x == y then val else Var y
substitute x val (Array es) = Array $ map (substitute x val) es
substitute x val (Tuple es) = Tuple $ map (substitute x val) es
substitute x val (Index arr idx) = Index (substitute x val arr) (substitute x val idx)
substitute x val (Lambda y body) = if x == y then Lambda y body else Lambda y (substitute x val body)
substitute x val (App f arg) = App (substitute x val f) (substitute x val arg)
substitute x val (If cond thenExpr elseExpr) = If (substitute x val cond) (substitute x val thenExpr) (substitute x val elseExpr)
substitute x val (BinOp op e1 e2) = BinOp op (substitute x val e1) (substitute x val e2)
substitute x val (Map e) = Map (substitute x val e)
substitute x val (Iota n) = Iota (substitute x val n)
substitute x val (Size arr) = Size (substitute x val arr)
substitute _ _ e = e

-- Apply a binary operation
applyOp :: BinOp -> Int -> Int -> Int
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = div
applyOp Mod = mod
