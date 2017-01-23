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
  | Identifier String
  deriving (Show, Eq)

-- Parser expressions
-- data Exp
--   = Let String Exp Exp
--   | Exp1 Exp1
--   deriving (Show,Eq)
--
-- data Exp1
--   = Plus Exp1 Term
--   | Minus Exp1 Term
--   | Term Term
--   deriving (Show,Eq)
--
-- data Term
--   = Times Term Factor
--   | Div Term Factor
--   | Factor Factor
--   deriving (Show,Eq)
--
-- data Factor
--   = Int Int
--   | Var String
--   | Brack Exp
--   deriving (Show,Eq)
