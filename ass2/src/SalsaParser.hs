module SalsaParser where

import SimpleParse
import Data.Char

{---- SYNTAX ----

Program    ::= Commands

Commands   ::= Command
             | Command Commands

Command    ::= Idents '->' Pos
             | 'toggle' Ident
             | ShapeDef
             | 'hidden' ShapeDef
             | Command '||' Command

ShapeDef   ::= 'rectangle' Ident Expr Expr Expr Expr Colour
             | 'circle' Ident Expr Expr Expr Colour

Idents     ::= Ident
             | Ident Idents

Pos        ::= '('Expr',' Expr ')'
             | '+' '('Expr',' Expr ')'

Expr       ::= Prim
             | Expr '*' Prim
             | Expr '/' Prim
             | Expr '+' Prim
             | Expr '-' Prim
Prim       ::= integer
             | Ident '.' 'x'
             | Ident '.' 'y'
             | '(' Expr ')'
Colour     ::= 'blue' | 'plum' | 'red' | 'green' | 'orange'

------------}


integerParser :: Parser Integer
integerParser = do
    i <- munch1 $ isDigit
    return $ read i  -- Not completely safe as read may crash

identParser :: Parser String
identParser = do
    s <- munch1 $ \ c -> (isLetter c || c == '_' || isDigit c)
    if any (\ a -> s == a) ["rectangle","circle","hidden","toggle"]
        then reject else return s
    




