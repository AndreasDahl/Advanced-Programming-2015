module SalsaInterp where

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n (pStartX, pStartY) (pEndX, pEndY) = iteration n ((pEndX - pStartX) `quot` n) ((pEndY - pStartY) `quot` n) (pStartX, pStartY)
    where
        iteration 0 _ _ _ = []
        iteration steps xStep yStep (xPrev, yPrev) = (xPrev + xStep, yPrev + yStep) : (iteration (steps - 1) xStep yStep (xPrev + xStep, yPrev + yStep))

