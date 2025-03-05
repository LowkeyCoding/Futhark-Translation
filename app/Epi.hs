module Epi where
import Binop
-- Eπ Syntax
data Process
  = Nul -- Inactive process
  | Send Channel [Term] Process -- Send terms on a channel
  | Recv Channel [String] Process -- Receive terms on a channel
  | Broad Channel [Term] Process -- Broadcat terms on a channel
  | Par Process Process -- Parrallel composition
  | Rep Process -- Replication
  | Res String Process -- Restriction a channel name (νa.P)
  | Match Term Term Comparison Process Process -- Match construct [T1 ⋈ T2] P Q
  deriving (Show, Eq)

data Channel
  = Name String -- Channel name (a)
  | Variable String -- Channel variable (x)
  | Labelled Channel String -- Labelled channel (a·l or x·l)
  deriving (Show, Eq)

data Term
  = Number Int -- Numeric term (n)
  | TChan Channel -- Channel as a term
  | BinaryOp BinOp Term Term -- Binary operation (T1 ⊙ T2)
  deriving (Show, Eq)

data Comparison = Eq | Neq | Lt | Gt | Leq | Geq -- Comparison operators
  deriving (Show, Eq)
