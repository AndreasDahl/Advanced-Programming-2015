module SalsaInterp where

import Control.Applicative ( Applicative(..) )

import           Data.List
import           Gpx
import           SalsaAst

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate i (ax, ay) (bx, by) = next (i-1) [(bx, by)]
    where
        dx = (bx - ax) `quot` i
        dy = (by - ay) `quot` i
        next :: Integer -> [Position] -> [Position]
        next 0 ps = ps
        next n s@((px,py):_) = next (n-1) ((px-dx, py-dy):s)
        next _ [] = [] -- can't happen

data Shape = Rectangle Position (Integer, Integer) Colour Bool
           | Circle Position Integer Colour Bool
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

getPosition :: Shape -> Position
getPosition (Rectangle p _ _ _) = p
getPosition (Circle p _ _ _ ) = p

addShape :: Context -> Ident -> Shape -> Context
addShape (Con n shapes) i shape = Con n ((i, shape) : shapes)

shapeLookup :: Context -> Ident -> Either String Shape
shapeLookup (Con _ shapes) needle = case find (\ (i, _) -> needle == i) shapes of
    Nothing -> Left $ "Could not find " ++ needle ++ " in context"
    Just (_, s) -> Right s

eval :: Context -> Expr -> Either String Integer
eval _ (Const i) = return i
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
    Right (Rectangle (x, _) _ _ _) -> return x
    Right (Circle (x, _) _ _ _)    -> return x
    Left e                         -> Left e
eval c (Yproj i) = case shapeLookup c i of
    Right (Rectangle (_, y) _ _ _) -> return y
    Right (Circle (_, y) _ _ _)    -> return y
    Left e                         -> Left e


astToShape :: Context -> Command -> Either String Shape
astToShape context (Rect _ x y w h c v) = do
    x' <- eval context x
    y' <- eval context y
    w' <- eval context w
    h' <- eval context h
    return (Rectangle (x', y') (w', h') c v)
astToShape context (Circ _ x y r c v) = do
    x' <- eval context x
    y' <- eval context y
    r' <- eval context r
    return (Circle (x', y') r' c v)
astToShape _ _ = Left "Command cannot be converted to a shape"


toggleShape :: Shape -> Shape
toggleShape (Rectangle p s c b) = Rectangle p s c (not b)
toggleShape (Circle p r c b)    = Circle p r c (not b)


contextFrame :: Context -> Frame
contextFrame (Con _ []) = []
contextFrame (Con n ((_, shape):shapes)) = case drawShape shape of
        Nothing -> contextFrame (Con n shapes)
        Just x' -> x' : contextFrame (Con n shapes)
    where
        drawShape :: Shape -> Maybe GpxInstr
        drawShape (Rectangle (x, y) (w, h) c True) = return $ DrawRect x y w h (show c)
        drawShape (Circle (x, y) r c True) = return $ DrawCirc x y r (show c)
        drawShape _ = Nothing


contextAnimation :: Context -> Animation
contextAnimation c@(Con n _) = replicate (fromIntegral n) (contextFrame c)

updateShape :: Shape -> (Integer, Integer) -> Shape
updateShape (Rectangle _ (w, h) c b) (newX, newY)
    = Rectangle (newX, newY) (w, h) c b
updateShape (Circle _ r c b) (newX, newY)
    = Circle (newX, newY) r c b


updateContext :: Context -> (Ident, Shape) -> Context
updateContext (Con n shapes) (newId, newShape) =
    Con n (map (\ (oldId, oldShape) -> if newId == oldId then (newId, newShape) else (oldId, oldShape)) shapes)

move :: Context -> Ident -> Pos -> Either String (Context, Animation)
move c@(Con n _) i (Abs x y) = do
    x' <- eval c x
    y' <- eval c y
    shape <- shapeLookup c i
    return $ fn c shape (interpolate n (getPosition shape) (x', y'))
    where
        fn :: Context -> Shape -> [Position] -> (Context, Animation)
        fn c' _ [] = (c', [])
        fn c' shape (p:ps) =
            let newShape = updateShape shape p in
            let newCon = updateContext c' (i, newShape) in
                case fn newCon newShape ps of
                    (finalC, as) -> (finalC, contextFrame newCon : as)
move c@(Con n _) i (Rel x y) = do
    x' <- eval c x
    y' <- eval c y
    shape <- shapeLookup c i
    let (shapeX, shapeY) = getPosition shape in return $ fn c shape (interpolate n (shapeX, shapeY) (x' + shapeX, y' + shapeY))
    where
        fn :: Context -> Shape -> [Position] -> (Context, Animation)
        fn c' _ [] = (c', [])
        fn c' shape (p:ps) =
            let newShape = updateShape shape p in
            let newCon = updateContext c' (i, newShape) in
                case fn newCon newShape ps of
                    (finalC, as) -> (finalC, contextFrame newCon : as)


command :: Command -> Salsa ()
command rect@(Rect i _ _ _ _ _ _) = Salsa $ \ con -> do
        shape <- astToShape con rect
        let newCon = addShape con i shape in return ((), newCon, contextAnimation newCon)
command circ@(Circ i _ _ _ _ _) = Salsa $ \ con -> do
        shape <- astToShape con circ
        let newCon = addShape con i shape in return ((), newCon, contextAnimation newCon)
command (Toggle needle) = Salsa $ \ (Con n shapes) ->
    let newCon = Con n (map (\ (i, shape) -> (if needle == i
        then (i, toggleShape shape)
        else (i, shape))) shapes)
    in return ((), newCon, [])
command (Move i p) = Salsa $ \ con -> do
        (newCon, animation)<- move con i p
        return ((), newCon, animation)
command (Par c1 c2) = Salsa $ \ con -> do
        (_, c1', _) <- runSalsa (command c1) con
        r <- runSalsa (command c2) c1'
        return r

prog :: Program
prog = [
    (Rect "jens" (Const 0) (Const 100) (Const 100) (Const 100) Blue True),
    Par (Circ "john" (Const 100) (Const 100) (Const 50) Red True)
    (Move "jens" (Abs (Const 200) (Const 200)))
    ]

runProg :: Integer -> Program -> Either String Animation
runProg n = fn (Con n [])
    where
        fn :: Context -> Program -> Either String Animation
        fn _ [] = return []
        fn context (f:fs) = do
            (_, context', f') <- runSalsa (command f) context
            a <- fn context' fs
            return (f' ++ a)
