module Language where

-- Expression data type
data Exp
  = Neg Exp
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
