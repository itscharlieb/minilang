module Compiler.SymbolTable where


import Data.List(find)


-- symbol table is a list of name->type mappings
type SymbolTable = [(String, Type)]


-- Symbol type
type Symbol = (String, Type)


-- variable types
data Type
  = IntType
  | FloatType
  | StringType
  deriving (Show, Eq)


--
empty :: SymbolTable
empty = []


--
insert :: SymbolTable -> Symbol -> Either String SymbolTable
insert table (name, t) =
  case find (\(n, _) -> n == name) table of
    Just _ -> Left $ "Symbol " ++ name ++ " already exists"
    Nothing -> Right $ (name, t):table


--
get :: SymbolTable -> String -> Maybe Symbol
get [] _ = Nothing
get ((n, t):st) name =
  if n == name then Just (n, t)
  else get st name
