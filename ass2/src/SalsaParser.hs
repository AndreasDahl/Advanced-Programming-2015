module SalsaParser where
    
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