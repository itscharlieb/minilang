{
{-# OPTIONS -w #-}

module Parser ( parse ) where

import Language
import Lexer
}

%name calc
%tokentype { Token }
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

Dclr  : var id ':' int ';'    { IntId $2 }
      | var id ':' float ';'  { FloatId $2 }
      | var id ':' string ';' { StringId $2 }

Stmts : Stmts Stmt            { $2 : $1 }
      | {- empty -}           { [] }

Stmt  : Smpl ';'              { $1 }
      | Ctrl                  { $1 }

Smpl  : print Exp             { Print $2 }
      | read id               { Read $2 }
      | id '=' Exp            { Assign $1 $3 }
      | Exp                   { Exp $1 }

Ctrl  : while Exp do
          Stmts
        done                  { While $2 $4}
      | if Exp then
          Stmts
        endif                 { If $2 $4 }
      | if Exp then
          Stmts
        else
          Stmts
        endif                 { IfElse $2 $4 $6 }

Exp   : Exp '+' Exp           { Plus $1 $3 }
      | Exp '-' Exp           { Minus $1 $3 }
      | Exp '*' Exp           { Times $1 $3 }
      | Exp '/' Exp           { Div $1 $3 }
      | '(' Exp ')'           { Bracketed $2 }
      | '-' Exp %prec NEG     { Negate $2 }
      | intVal                { Int $1 }
      | floatVal              { Float $1 }
      | stringVal             { String $1 }
      | id                    { Id $1 }

{

-- Parser errors
parseError :: [Token] -> a
parseError _ = error "Parse error"


-- Runs calc parser
parse :: String -> Program
parse = calc . lexer

}
