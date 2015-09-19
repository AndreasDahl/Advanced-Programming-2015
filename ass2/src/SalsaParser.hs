module SalsaParser where

import Control.Monad ( MonadPlus(..), liftM )
import Control.Applicative ( Applicative(..), Alternative((<|>), empty, many) )
import Data.Char

import SalsaAst
import SimpleParse



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
    i <- token $ munch1 isDigit
    return $ read i  -- Not completely safe as read may crash

identParser :: Parser Ident
identParser = do
    s <- token $ munch1 $ \ c -> isLetter c || c == '_' || isDigit c
    if firstIsInt s || any (\ a -> s == a) ["rectangle","circle",
            "hidden","toggle","blue","plum","red","green","orange"]
        then reject else return s
    where
        firstIsInt (s:_) = isDigit s
        firstIsInt []    = False

identsParser :: Parser [Ident]
identsParser = many1 identParser 

colourParser :: Parser Colour
colourParser = (symbol "blue" >> return Blue) 
           <|> (symbol "plum" >> return Plum)
           <|> (symbol "red"  >> return Red)
           <|> (symbol "green" >> return Green)
           <|> (symbol "orange" >> return Orange)

exprParser :: Parser Expr
exprParser = reject

primParser :: Parser Expr
primParser = do
    p <- constIP <|> (proj 'x') <|> (proj 'y') <|> expr
    return p
    where
        constIP = do
            i <- integerParser
            return $ Const i
        proj c = do
            i <- identParser
            _ <- (schar '.') >> (schar c)
            if c == 'x' then return $ Xproj i else return $ Yproj i
        expr = do
            _ <- schar '('
            e <- exprParser
            _ <- schar ')'
            return e



