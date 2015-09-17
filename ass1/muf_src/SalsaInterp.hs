{-
What is instance????
We have:
data Nat = Zero
         | Succ Nat
    deriving (Show, Eq, Ord)

prelude:
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not(x == y)

Want to:
instance Eq Nat where
    Zero == Zero = True
    Succ n == Succ m = n == m
    _ == _ = False





Functor takes a function and a class, and uses the function and the inside of the class

prelude:
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Nat where
-- fmap :: (a -> b) -> KenList a -> KenList b
    fmap _ Zero = Zero
    fmap f (Succ nat) = Succ (f nat)



class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    return :: a -> Maybe a
    >>= :: Maybe a -> (a -> Maybe b) -> Maybe b

    (return x) = Just x
    (m >>= f) =
        case m of
            Just x -> f x
            Nothing -> Nothing

-}

module SalsaInterp where

import Data.List
import Gpx
import SalsaAst

type Position = (Integer, Integer)

-- 3 (0,0) (10,10) -> [(4,4) (7,7) (10,10)]
interpolate :: Integer -> Position -> Position -> [Position]
interpolate i (ax, ay) (bx, by) = next (i-1) [(bx, by)]
    where
        dx = (bx - ax) `quot` i
        dy = (by - ay) `quot` i
        next :: Integer -> [Position] -> [Position]
        next 0 ps = ps
        next n s@((px,py):_) = next (n-1) ((px-dx, py-dy):s)
        next _ [] = [] -- can't happen

data Shape = SRect Position Colour
           | SCirc Integer Colour

-- Each shape’s position and visibility, and the framerate
data Context = Context Integer [(Ident, Shape, Position, Bool)]

newtype Salsa a = Salsa {
    runSalsa :: Context -> Either String (a, Context, Animation) }

--instance Monad Salsa where
    -- The function (return a) takes a context and returns (Right((a, context, [])))
--    return a = Salsa $ \ context -> Right (a, context, [])




evalExpr :: Context -> Expr -> Either String Integer
evalExpr c (Plus e1 e2) = do
    e1' <- evalExpr c e1
    e2' <- evalExpr c e2
    return (e1' + e2')
evalExpr c (Minus e1 e2) = do
    e1' <- evalExpr c e1
    e2' <- evalExpr c e2
    return (e1' + e2')
evalExpr c (Mult e1 e2) = do
    e1' <- evalExpr c e1
    e2' <- evalExpr c e2
    return (e1' * e2')
evalExpr c (Div e1 e2) = do
    e1' <- evalExpr c e1
    e2' <- evalExpr c e2
    if e2' == 0 then Left "Division by 0" else Right (e1' `quot` e2')
evalExpr _ (Const i) = return i
evalExpr (Context _ ss) (Xproj s) =
    case find ((\ s' (a,_,_,_) -> a == s') s) ss of
        Just (_,_,(x,_),_) -> Right x
        Nothing -> Left ("Shape not found: " ++ s)
evalExpr (Context _ ss) (Yproj s) =
    case find ((\ s' (a,_,_,_) -> a == s') s) ss of
        Just (_,_,(_,y),_) -> Right y
        Nothing -> Left ("Shape not found: " ++ s)







{-
data Command = Rect Ident Expr Expr Expr Expr Colour Bool
             | Circ Ident Expr Expr Expr Colour Bool
             | Move Ident Pos
             | Toggle Ident
             | Par Command Command
-}


--eval c (Xproj s) = case find ((\ s (_,a,_) -> a == s) s) c of
--    Just x -> return x
--    Nothing -> return ("Shape not found: " ++ s)




--command :: Command -> Salsa ()
--command Rect = Salsa (\ ) 
--command Rect Ident Expr Expr Expr Expr Colour Bool = undefined
--command Circ Ident Expr Expr Expr Colour Bool = undefined
--command Move Ident Pos = undefined
--command Toggle Ident = undefined
--command Par Command Command = undefined


--interp :: Integer -> [Salsa] -> Either String Animation


--runProg :: Integer -> Program -> Either String Animation
--runProg _ [] = Right []
--runProg n (p:ps) = Salsa Context 
    
