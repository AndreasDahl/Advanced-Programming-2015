module SalsaInterp where

import SalsaAst
import Gpx
import Data.List

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n (pStartX, pStartY) (pEndX, pEndY)
    = iteration n
        ((pEndX - pStartX) `quot` n)
        ((pEndY - pStartY) `quot` n)
        (pStartX, pStartY)
    where
        iteration 0 _ _ _ = []
        iteration steps xStep yStep (xPrev, yPrev)
            = let next = (xPrev + xStep, yPrev + yStep) in
                next
                : iteration
                    (steps - 1)
                    xStep
                    yStep
                    next

data Shape = Rectangle Position Bool
           | Circle Position Bool
    deriving(Show, Eq)

data Context = Con Integer [(Ident, Shape)]

newtype Salsa a =
    Salsa { runSalsa :: Context -> Either String (a, Context, Animation) }

instance Monad Salsa where
    return a = Salsa $ \ context -> Right (a, context, [])
    sa >>= f = Salsa $ \ con ->
        case runSalsa sa con of
            Left s -> Left s
            Right (a, c, _) -> runSalsa (f a) c
    fail msg = Salsa $ \ _ -> Left msg

instance Functor Salsa where
  fmap f xs = xs >>= return . f

instance Applicative Salsa where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

addShape :: Context -> Ident -> Shape -> Context
addShape (Con n shapes) id shape = Con n ((id, shape) : shapes)

shapeLookup :: Context -> Ident -> Either String Shape
shapeLookup (Con _ shapes) needle = case find (\ (i, _) -> needle == i) shapes of
    Nothing -> Left $ "Could not find " ++ needle ++ " in context"
    Just (_, s) -> Right s

eval :: Context -> Expr -> Either String Integer
eval c (Const i) = return i
eval c (Plus a b) = do
    x <- eval c a
    y <- eval c b
    return (x + y)
eval c (Minus a b) = do
    x <- eval c a
    y <- eval c b
    return $ x - y
eval c (Mult a b) = do
    x <- eval c a
    y <- eval c b
    return $ x * y
eval c (Div a b) = do
    x <- eval c a
    y <- eval c b
    return $ x `quot` y
eval c (Xproj i) = case shapeLookup c i of
    Right (Rectangle (x, _) _) -> return x
    Right (Circle (x, _) _)    -> return x
    Left e                     -> Left e
eval c (Yproj i) = case shapeLookup c i of
    Right (Rectangle (_, y) _) -> return y
    Right (Circle (_, y) _)    -> return y
    Left e                     -> Left e


astToShape :: Context -> Command -> Either String Shape
astToShape context (Rect i x y w h c v) = do
    x' <- eval context x
    y' <- eval context y
    return (Rectangle (x', y') v)
astToShape context (Circ i x y r c v) = do
    x' <- eval context x
    y' <- eval context y
    return (Circle (x', y') v)



command :: Command -> Salsa ()
command rect@(Rect i x y w h c v) = Salsa $ \ con -> do
        shape <- astToShape con rect
        return ((), addShape con i shape, [])
command circ@(Circ i x y r c v) = Salsa $ \ con -> do
        shape <- astToShape con circ
        return ((), addShape con i shape, [])
command (Toggle i) = Salsa $ \ con -> return ((), con, [])
--command Toggle t = undefined
--command Par p = undefined
command _ = undefined
