module Language where

-- Program root data type
data Program
  = Program [Dclr] [Stmt]
  deriving (Show, Eq)

-- Declerations
data Dclr
  = IntId String
  | FloatId String
  | StringId String
  deriving (Show, Eq)

-- Statements
data Stmt
  = Print Exp
  | Read String
  | Assign String Exp
  | While Exp [Stmt]
  | If Exp [Stmt]
  | IfElse Exp [Stmt] [Stmt]
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
