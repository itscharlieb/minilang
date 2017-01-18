{
{-# OPTIONS -w #-}

module Parser ( parse ) where

import Language
import Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      let             { Token _ TokenLet }
      in              { Token _ TokenIn }
      int             { Token _ (TokenInt $$) }
      var             { Token _ (TokenVar $$) }
      '='             { Token _ TokenEq }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{

-- Parser errors
parseError :: [Token] -> a
parseError _ = error "Parse error"


-- Runs calc parser
parse :: String -> Exp
parse = calc . lexer

}
