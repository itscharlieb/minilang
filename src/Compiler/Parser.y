{
{-# OPTIONS -w #-}

module Compiler.Parser ( parse ) where

import Compiler.Language
import Compiler.Lexer
}

%name calc
%tokentype { Token }

%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { parseError }

%left '+' '-'
%left '*' '/'
%left NEG

%token
      var             { Token _ TokenVar }
      id              { Token _ (TokenId $$)}
      int             { Token _ TokenIntType }
      float           { Token _ TokenFloatType }
      string          { Token _ TokenStringType }

      intVal          { Token _ (TokenIntVal $$) }
      floatVal        { Token _ (TokenFloatVal $$) }
      stringVal       { Token _ (TokenStringVal $$) }

      print           { Token _ TokenPrint }
      read            { Token _ TokenRead }

      while           { Token _ TokenWhile }
      do              { Token _ TokenDo }
      done            { Token _ TokenDone }

      if              { Token _ TokenIf }
      then            { Token _ TokenThen}
      else            { Token _ TokenElse}
      endif           { Token _ TokenEndif}

      '='             { Token _ TokenEq}
      ';'             { Token _ TokenSemicolon }
      ':'             { Token _ TokenColon}

      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

Program
      : Dclrs Stmts           { Program $1 $2 }

Dclrs : Dclrs Dclr            { $2 : $1 }
      | {- empty -}           { [] }

Dclr  : var id ':' int ';'    { Dclr $2 TInt }
      | var id ':' float ';'  { Dclr $2 TFloat }
      | var id ':' string ';' { Dclr $2 TString }

Stmts : Stmts Stmt            { $2 : $1 }
      | {- empty -}           { [] }

Stmt  : print Exp ';'         { Print $2 }
      | read id ';'           { Read $2 }
      | id '=' Exp ';'        { Assign $1 $3 }
      | while Exp do
          Stmts
        done                  { While $2 $4}
      | if Exp then
          Stmts
        endif                 { If $2 $4 [] }
      | if Exp then
          Stmts
        else
          Stmts
        endif                 { If $2 $4 $6 }

Exp   : Exp '+' Exp           { Plus $1 $3 }
      | Exp '-' Exp           { Minus $1 $3 }
      | Exp '*' Exp           { Times $1 $3 }
      | Exp '/' Exp           { Div $1 $3 }
      | '(' Exp ')'           { $2 }
      | '-' Exp %prec NEG     { Negate $2 }
      | intVal                { Int $1 }
      | floatVal              { Float $1 }
      | stringVal             { String $1 }
      | id                    { Id $1 }

{

-- Wrapper function
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)


--
parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parse error")


--
parse :: FilePath -> String -> Either String Program
parse = runAlex' calc

}
