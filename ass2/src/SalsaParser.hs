module SalsaParser where

import Control.Applicative ( Applicative(..), Alternative((<|>), empty, many) )
import Data.Char

import SalsaAst
import SimpleParse



{---- SYNTAX ----

Program    ::= Commands

Commands   ::= Command
             | Command Commands

Command    ::= CommandOpt
             | CommandOpt '||' Command
CommandOpt ::= Idents '->' Pos
             | 'toggle' Ident
             | ShapeDef
             | 'hidden' ShapeDef

ShapeDef   ::= 'rectangle' Ident Expr Expr Expr Expr Colour
             | 'circle' Ident Expr Expr Expr Colour

Idents     ::= Ident
             | Ident Idents

Pos        ::= '('Expr',' Expr ')'
             | '+' '('Expr',' Expr ')'

Expr       ::= Prim
             | Prim MulOp
             | Prim AddOp
MulOp      ::= '*' Expr
             | '/' Expr
AddOp      ::= '+' Expr
             | '-' Expr
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
exprParser = primParser `chainl1` mulOp `chainl1` addOp
    where
        mulOp = do{ _ <- schar '*'; return Mult  }
            <|> do{ _ <- schar '/'; return Div   }
        addOp = do{ _ <- schar '+'; return Plus  }
            <|> do{ _ <- schar '-'; return Minus }

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

shapeDefParser :: Bool -> Parser Command
shapeDefParser hidden = rectParser <|> circleParser
    where
        rectParser = do
            _ <- symbol "rectangle"
            i <- identParser
            x <- exprParser
            y <- exprParser
            w <- exprParser
            h <- exprParser
            c <- colourParser
            return $ Rect i x y w h c hidden
        circleParser = do
            _ <- symbol "circle"
            i <- identParser
            x <- exprParser
            y <- exprParser
            r <- exprParser
            c <- colourParser
            return $ Circ i x y r c hidden

commandParser :: Parser Command
commandParser = commandOpt <|> do
    c1 <- commandOpt
    _  <- symbol "||"
    c2 <- commandParser
    return $ Par c1 c2
    where
        commandOpt :: Parser Command
        commandOpt = (do
                _ <- symbol "toggle" >> space
                i <- identParser
                return $ Toggle i) <|>
            shapeDefParser True <|>
            (symbol "hidden" >> space >> shapeDefParser False) <|> (do
                ids <- identsParser
                _   <- symbol "->"
                pos <- posParser
                return $ foldl1 Par (map (`Move` pos) ids))

commandsParser :: Parser [Command]
commandsParser = commandParser `sepBy1` space

type Error = String

parseString :: String -> Either Error Program
parseString iput = let ret = do { cs <- commandsParser; _ <- spaces; return cs } in
    case parseEof ret iput of
    [(prog, "")] -> return prog
    _ -> Left "Fail"  -- TODO: Better error message
    where


parseFile :: FilePath -> IO (Either Error Program)
parseFile fp = do
    str <- readFile fp
    return $ parseString str
