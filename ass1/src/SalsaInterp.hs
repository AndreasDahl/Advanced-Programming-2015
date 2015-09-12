module SalsaInterp where

type ColourName = String
type Frame = [GpxInstr]
type Animation = [Frame]
data GpxInstr = DrawRect Integer Integer Integer Integer ColourName
              | DrawCirc Integer Integer Integer ColourName
              deriving (Eq, Show)

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

type Context = Integer

newtype Salsa a =
    Salsa { runSalsa :: Context -> Either String (a, Context, Animation) }

instance Monad Salsa where
    return a = Salsa $ \ context -> Right (a, context, [])
    _ >>= _ = undefined

instance Functor Salsa where
  fmap f xs = xs >>= return . f

instance Applicative Salsa where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f
