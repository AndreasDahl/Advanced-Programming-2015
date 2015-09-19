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

{-

ExprE      ::= ExprT ExprEopt
ExprEopt   ::= '+' ExprT ExprEopt
             | '-' ExprT ExprEopt
             | None
ExprT      ::= Prim ExprTopt
ExprTopt   ::= '*' Prim ExprTopt
             | '/' Prim ExprTopt
             | None
Prim       ::= integer
             | Ident '.' 'x'
             | Ident '.' 'y'
             | '(' Expr ')'

-}


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

primParser :: Parser Expr
primParser = constIP <|> proj 'x' <|> proj 'y' <|> expr
    where
        constIP = do
            i <- integerParser
            return $ Const i
        proj c = do
            i <- identParser
            _ <- schar '.' >> schar c
            if c == 'x' then return $ Xproj i else return $ Yproj i
        expr = do
            _ <- schar '('
            e <- exprParser
            _ <- schar ')'
            return e

exprParser :: Parser Expr
exprParser = primParser 
    <|> (do
        p <- primParser
        exprOptParser p)
    where
        exprOptParser :: Expr -> Parser Expr
        exprOptParser p1 = (do 
            _ <- schar '*' 
            p2 <- exprParser
            return $ Mult p1 p2) <|> (do 
            _ <- schar '/' 
            p2 <- exprParser
            return $ Div p1 p2) <|> (do 
            _ <- schar '+' 
            p2 <- exprParser
            return $ Plus p1 p2) <|> (do 
            _ <- schar '-' 
            p2 <- exprParser
            return $ Minus p1 p2) 

posParser :: Parser Pos
posParser = (do
    (e1, e2) <- getExprs
    return $ Abs e1 e2) 
        <|> (do
    _        <- schar '+'
    (e1, e2) <- getExprs
    return $ Rel e1 e2)
    where
        getExprs :: Parser (Expr, Expr)
        getExprs = do
            _ <- schar '('
            e1 <- exprParser
            _ <- schar ','
            e2 <- exprParser
            _ <- schar ')'
            return (e1, e2)




