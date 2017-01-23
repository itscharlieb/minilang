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
      int             { Token _ (TokenIntVal $$) }
      float           { Token _ (TokenFloatVal $$) }
      string          { Token _ (TokenStringVal $$) }
      id              { Token _ (TokenId $$)}

      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }

      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

Exp   : Exp '+' Exp           { Plus $1 $3 }
      | Exp '-' Exp           { Minus $1 $3 }
      | Exp '*' Exp           { Times $1 $3 }
      | Exp '/' Exp           { Div $1 $3 }
      | '(' Exp ')'           { Bracketed $2 }
      | '-' Exp %prec NEG     { Negate $2 }
      | int                   { Int $1 }
      | float                 { Float $1 }
      | string                { String $1 }
      | id                    { Id $1 }

{

-- Parser errors
parseError :: [Token] -> a
parseError _ = error "Parse error"


-- Runs calc parser
parse :: String -> Exp
parse = calc . lexer

}
