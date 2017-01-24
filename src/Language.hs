module Language where

data Stmt
  = Print Exp
  | Read String
  | Assign String Exp
  | Exp Exp
  deriving (Show, Eq)

-- Expression data type
data Exp
  = Negate Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Bracketed Exp
  | Int Int
  | Float Float
  | String String
  | Id String
  deriving (Show, Eq)
