{
-- only list imports here
import Data.Char
import Tree 
}

%tokentype { Token }

%lexer { lexer } { TokenEOF }

%token
	'*' 	{ Sym '*' }
	'+' 	{ Sym '+' }
	'-' 	{ Sym '-' }
	'(' 	{ Sym '(' }
	')' 	{ Sym ')' }
	i 	{ AnInt $$ }

%%

E :: {Tree ForestId Int}
 : E '+' E2	{ Plus  $1 $3 }
 | E '-' E2  { Minus $1 $3 }
 | i            { Const $1 }

E2 :: {Tree ForestId Int}
 : E2 '*' E2	{ Times $1 $3 }
 | '(' E ')'    { Pars  $2 } 
 | i            { Const $1 }



{

data Token
	= TokenEOF
	| Sym Char
	| AnInt {getInt :: Int}
  deriving (Show,Eq, Ord)


lexer :: String -> [Token]
lexer [] = []
lexer (' ':cs) = lexer cs

lexer (c:cs) | c `elem` "+*-()" 
 = Sym c : lexer cs

lexer (c:cs) | isDigit c 
 = let (yes,no) = span isDigit cs in AnInt (read $ c:yes) : lexer no

}
