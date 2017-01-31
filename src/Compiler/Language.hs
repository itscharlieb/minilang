module Compiler.Language where


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
  | Int Int
  | Float Float
  | String String
  | Id String
  deriving (Show, Eq)


--
pretty :: Program -> String
pretty (Program dclrs stmts) =
  prettyDclrs dclrs ++ "\n" ++ prettyStmts stmts ""


--
prettyDclrs :: [Dclr] -> String
prettyDclrs [] = ""
prettyDclrs (dclr:dclrs) =
  (prettyDclr dclr) ++ "\n" ++ (prettyDclrs dclrs)


--
prettyDclr :: Dclr -> String
prettyDclr (IntId name) = "var " ++ name ++ ": int;"
prettyDclr (FloatId name) = "var " ++ name ++ ": float;"
prettyDclr (StringId name) = "var " ++ name ++ ": string;"


--
prettyStmts :: [Stmt] -> String -> String
prettyStmts [] _ = ""
prettyStmts (stmt:stmts) indent =
  indent ++ (prettyStmt stmt indent) ++ "\n" ++ (prettyStmts stmts indent)


prettyStmt :: Stmt -> String -> String
prettyStmt (Print e) _ = "print " ++ prettyExp e ++ ";"
prettyStmt (Read name) _ = "read " ++ name ++ ";"
prettyStmt (Assign name e) _ = name ++ " = " ++ prettyExp e ++ ";"
prettyStmt (While e stmts) indent =
  "while " ++ prettyExp e ++ " do\n"
    ++ prettyStmts stmts ('\t':indent) ++ indent ++ "done"
prettyStmt (If e stmts) indent =
  "if " ++ prettyExp e ++ " then\n"
    ++ prettyStmts stmts ('\t':indent) ++ "endif"
prettyStmt (IfElse e stmts1 stmts2) indent =
  "if " ++ prettyExp e ++ " then\n"
    ++ prettyStmts stmts1 ('\t':indent) ++ indent ++ "else\n"
    ++ prettyStmts stmts2 ('\t':indent) ++ indent ++ "endif"
prettyStmt (Exp e) _ = prettyExp e


--
prettyExp :: Exp -> String
prettyExp (Negate e) = "-" ++ prettyExp e
prettyExp (Plus e1 e2) = prettyBinary e1 e2 "+"
prettyExp (Minus e1 e2) = prettyBinary e1 e2 "-"
prettyExp (Times e1 e2) = prettyBinary e1 e2 "*"
prettyExp (Div e1 e2) = prettyBinary e1 e2 "/"
prettyExp (Int i) = show i
prettyExp (Float f) = show f
prettyExp (String s) = s
prettyExp (Id s) = s


prettyBinary :: Exp -> Exp -> String -> String
prettyBinary e1 e2 op =
  "(" ++ (prettyExp e1) ++ " " ++ op ++ " " ++ (prettyExp e2) ++ ")"
