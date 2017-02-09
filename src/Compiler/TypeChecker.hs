module Compiler.TypeChecker where


import Compiler.Language
import Compiler.SymbolTable


-- Simple solution to having a typed AST
-- Duplicate entire AST with typed expressions
data TProgram
  = TProgram [TDclr] [TStmt]
  deriving (Show, Eq)


data TDclr
  = TDclr String PrimType
  deriving (Show, Eq)


data TStmt
  = TPrint TExp
  | TRead String
  | TAssign String TExp
  | TWhile TExp [TStmt]
  | TIf TExp [TStmt] [TStmt]
  | TExp TExp
  deriving (Show, Eq)


type TExp = (TExp', PrimType)


data TExp'
  = TNegate TExp
  | TPlus TExp TExp
  | TMinus TExp TExp
  | TTimes TExp TExp
  | TDiv TExp TExp
  | TIntId Int
  | TFloatId Float
  | TStringId String
  | TId String
  deriving (Show, Eq)


data TypeError
  = TypeMismatch
    { expected :: PrimType
    , found :: PrimType
    }
  | InvalidType
    { found :: PrimType }
  | MissingDeclaration
    { declaration :: String }
  | DuplicateDeclaration
    { declaration :: String }
  deriving (Eq)


instance Show TypeError where
  show TypeMismatch {expected = e,  found = f} =
    concat ["Expected Type = ", show e, ". Found Type = ", show f]
  show InvalidType {found = f} =
    "Invalid Type = " ++ show f
  show MissingDeclaration {declaration = d} =
    "Missing Declaration for " ++ show d
  show DuplicateDeclaration {declaration = d} =
    "Duplication Declaration for " ++ show d


-- Output SymbolTable for assignment requirements
typeCheck :: Program -> Either TypeError TProgram
typeCheck (Program dclrs stmts) = do
  table <- buildTable dclrs
  tstmts <- validateStatements stmts table
  let tdclrs = map (\(Dclr name type') -> TDclr name type') dclrs in
    Right $ TProgram tdclrs tstmts


--
validateStatements :: [Stmt] -> SymbolTable -> Either TypeError [TStmt]
validateStatements stmts table = mapM (`validateStatement` table) stmts


--
validateStatement :: Stmt -> SymbolTable -> Either TypeError TStmt
validateStatement (Print e) table = do
  e' <- validateExpression e table
  return $ TPrint e'
validateStatement (Read name) table =
  case get table name of
    Nothing -> Left $ MissingDeclaration name
    Just _ -> Right $ TRead name
validateStatement (Assign name e) table =
  case get table name of
    Nothing -> Left $ MissingDeclaration name
    Just (_, nameType) -> do
      e' <- validateExpression e table
      if nameType == snd e'
        then Right $ TAssign name e'
        else Left $ TypeMismatch nameType (snd e')
validateStatement (While e stmts) table = do
  e' <- validateExpression e table
  stmts' <- validateStatements stmts table
  Right $ TWhile e' stmts'
validateStatement (If e stmts1 stmts2) table = do
  e' <- validateExpression e table
  stmts1' <- validateStatements stmts1 table
  stmts2' <- validateStatements stmts2 table
  Right $ TIf e' stmts1' stmts2'
validateStatement (Exp e) table = do
  e' <- validateExpression e table
  Right $ TExp e'


--
validateExpression :: Exp -> SymbolTable -> Either TypeError TExp
validateExpression (Negate e) table = do
  e' <- validateExpression e table
  case snd e' of
    TString -> Left $ InvalidType TString
    _ -> return e'
validateExpression (Plus e1 e2) table = validateStandardBinaryOp e1 e2 TPlus table
validateExpression (Minus e1 e2) table = validateStandardBinaryOp e1 e2 TMinus table
validateExpression (Times e1 e2) table = do
  e1' <- validateExpression e1 table
  e2' <- validateExpression e2 table
  let e = TTimes e1' e2' in
    case (snd e1', snd e2') of
      (TInt, TInt) -> Right (e, TInt)
      (TInt, TFloat) -> Right (e, TFloat)
      (TInt, TString) -> Right (e, TString)
      (TFloat, TInt) -> Right (e, TFloat)
      (TFloat, TFloat) -> Right (e, TFloat)
      (TString, TInt) -> Right (e, TString)
      (expectedType, actualType) -> Left $ TypeMismatch expectedType actualType
validateExpression (Div e1 e2) table = validateStandardBinaryOp e1 e2 TDiv table
validateExpression (Int i) _ = Right (TIntId i, TInt)
validateExpression (Float f) _ = Right (TFloatId f, TFloat)
validateExpression (String s) _ = Right (TStringId s, TString)
validateExpression (Id name) table =
  case get table name of
    Nothing -> Left $ MissingDeclaration name
    Just (_, type') -> Right (TId name, type')


--
validateStandardBinaryOp :: Exp -> Exp -> (TExp -> TExp -> TExp') -> SymbolTable -> Either TypeError TExp
validateStandardBinaryOp e1 e2 f table = do
  e1' <- validateExpression e1 table
  e2' <- validateExpression e2 table
  case (snd e1', snd e2') of
    (TInt, TInt) -> Right (f e1' e2', TInt)
    (TFloat, TInt) -> Right (f e1' e2', TFloat)
    (TInt, TFloat) -> Right (f e1' e2', TFloat)
    (TFloat, TFloat) -> Right (f e1' e2', TFloat)
    (TString, TString) -> Right (f e1' e2', TString)
    (expectedType, actualType) -> Left $ TypeMismatch expectedType actualType


--
buildTable :: [Dclr] -> Either TypeError SymbolTable
buildTable dclrs = buildTable' dclrs empty where
  buildTable' [] table = Right table
  buildTable' (dclr:dclrs') table =
    case insert' table dclr of
      Left errorMsg -> Left errorMsg
      Right table' -> buildTable' dclrs' table'


--
insert' :: SymbolTable -> Dclr -> Either TypeError SymbolTable
insert' table (Dclr name primType) =
  case insert table (name, primType) of
    Left _ -> Left $ DuplicateDeclaration name
    Right table' -> Right table'
