module Butf where 


-- AST for ButF expressions
data Expr
  = Const Int
  | Var String
  | Array [Expr]
  | Tuple [Expr]
  | Index Expr Expr
  | DelayedIndex [Expr] Int
  | Lambda String Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Ops (Ops Expr)
  deriving (Show, Eq)

-- Binary operations
data BinOp = Add | Sub | Mul | Div | Mod
  deriving (Show, Eq)

data Ops a 
  = Map a a
  | Iota a
  | Size a
  | BinOp BinOp a a
  deriving (Show, Eq)

-- Step function
step :: Expr -> Expr 
step (Const n) = Const n
step (Var x) = error $ "Unbound variable: " ++ x
step (Array es) = Array $ map step es
step (Tuple es) = Tuple $ map step es
step (Index arr idx) = case (step arr, step idx) of
  (Array arr', Const i) -> DelayedIndex arr' i  -- Delay the indexing operation
  _ -> error "Indexing error"
step (DelayedIndex arr i) = arr !! i 
step (Lambda x body) = Lambda x body
step (App (Lambda x body) arg) = step $ substitute x (step arg) body
step (If cond thenExpr elseExpr) = case step cond of
  Const 0 -> elseExpr
  _ -> thenExpr
step (Ops (BinOp op e1 e2)) = case (step e1, step e2) of
  (Const n1, Const n2) -> Const $ applyOp op n1 n2
  _ -> error "Binary operation error"
step (Ops (Map f arr)) = case step arr of
  Array arr' -> Array $ map (step . App f) arr'
  _ -> error "Map error"
step (Ops (Iota n)) = case step n of
  Const n' -> Array $ map Const [0..n'-1]
  _ -> error "Iota error"
step (Ops (Size arr)) = case step arr of
  Array arr' -> Const $ length arr'
  _ -> error "Size error"

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
eval (Ops (BinOp op e1 e2)) = case (eval e1, eval e2) of
  (Const n1, Const n2) -> Const $ applyOp op n1 n2
  _ -> error "Binary operation error"
eval (Ops (Map f arr)) = case eval arr of
  Array arr' -> Array $ map (eval . App f) arr'
  _ -> error "Map error"
eval (Ops (Iota n)) = case eval n of
  Const n' -> Array $ map Const [0..n'-1]
  _ -> error "Iota error"
eval (Ops (Size arr)) = case eval arr of
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
substitute x val (Ops (BinOp op e1 e2)) = Ops (BinOp op (substitute x val e1) (substitute x val e2))
substitute x val (Ops (Map f arr)) = Ops (Map (substitute x val f) (substitute x val arr))
substitute x val (Ops (Iota n)) = Ops (Iota (substitute x val n))
substitute x val (Ops (Size arr)) = Ops (Size (substitute x val arr))
substitute _ _ e = e

-- Apply a binary operation
applyOp :: BinOp -> Int -> Int -> Int
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = div
applyOp Mod = mod
