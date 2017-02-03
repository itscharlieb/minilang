module Compiler.Language where


--
data PrimType
  = TInt
  | TFloat
  | TString
  deriving (Show, Eq)


-- Program root data type
data Program
  = Program [Dclr] [Stmt]
  deriving (Show, Eq)


-- Declarations
data Dclr
  = Dclr String PrimType
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
  | Int Int
  | Float Float
  | String String
  | Id String
  deriving (Show, Eq)


-- prettyIndented typeclass
class Pretty a where
  pretty :: a -> String
  pretty a = prettyIndented a ""

  prettyList :: [a] -> String -> String
  prettyList xs indent = unlines $ map (`prettyIndented` indent) xs

  prettyIndented :: a -> String -> String
  prettyIndented a i = i ++ pretty a



--
instance Pretty Program where
  prettyIndented (Program dclrs stmts) indent =
    prettyList dclrs indent ++ "\n" ++ prettyList stmts indent


--
instance Pretty Dclr where
  pretty (Dclr name prim) = "var " ++ name ++ ": " ++ pretty prim ++ ";"


--
instance Pretty PrimType where
  pretty TInt = "int"
  pretty TFloat = "float"
  pretty TString = "string"


--
instance Pretty Stmt where
  prettyIndented (Print e) i = i ++ "print " ++ pretty e ++ ";"
  prettyIndented (Read name) i = i ++ "read " ++ name ++ ";"
  prettyIndented (Assign name e) i = i ++ name ++ " = " ++ pretty e ++ ";"
  prettyIndented (While e stmts) i = concat
    [ i ++ "while " ++ pretty e ++ " do\n"
    , prettyList stmts ('\t':i)
    , i ++ "done"
    ]
  prettyIndented (If e stmts) i = concat
    [ i ++ "if " ++ pretty e ++ " then\n"
    , prettyList stmts ('\t':i)
    , i ++ "endif"
    ]
  prettyIndented (IfElse e stmts1 stmts2) i = concat
    [ i ++ "if " ++ pretty e ++ " then\n"
    , prettyList stmts1 ('\t':i)
    , i ++ "else\n"
    , prettyList stmts2 ('\t':i)
    , i ++ "endif"
    ]
  prettyIndented (Exp e) i = i ++ pretty e


--
instance Pretty Exp where
  pretty (Negate e) = "-" ++ pretty e
  pretty (Plus e1 e2) = prettyBinary e1 e2 "+"
  pretty (Minus e1 e2) = prettyBinary e1 e2 "-"
  pretty (Times e1 e2) = prettyBinary e1 e2 "*"
  pretty (Div e1 e2) = prettyBinary e1 e2 "/"
  pretty (Int i) = show i
  pretty (Float f) = show f
  pretty (String s) = s
  pretty (Id s) = s


-- |Helper function that converts any binary operation to a prettyIndented string.
prettyBinary :: Exp -> Exp -> String -> String
prettyBinary e1 e2 op =
  "(" ++ pretty e1 ++ " " ++ op ++ " " ++ pretty e2 ++ ")"
