module Curves where

data Point = Point (Double, Double)
    deriving (Show)

point :: (Double, Double) -> Point
point (x, y) = Point (x, y)

pointX :: Point -> Double
pointX (Point (x, _)) = x

pointY :: Point -> Double
pointY (Point (_, y)) = y

instance Eq Point where
    (Point(xa, ya)) == (Point(xb, yb)) =
        abs(xa - xb) < 0.01 && abs(ya - yb) < 0.01


data Curve = Curve [Point]
    deriving (Show)

curve :: Point -> [Point] -> Curve
curve p ps = Curve (p:ps)

connect :: Curve -> Curve -> Curve
connect (Curve as) (Curve bs) = Curve (as ++ bs)

rotate :: Curve -> Double -> Curve
rotate (Curve as) r = Curve (map fr' as)
    where
        fr lcos lsin (Point (x,y)) = (point (x*lcos-y*lsin, x*lsin+y*lcos))
        fr' = (fr (cos (r/180.0*pi)) (sin (r/180.0*pi)))

translate :: Curve -> Point -> Curve
translate (Curve as) p = Curve (map (ft p) as)
    where
        ft (Point(xa, ya)) (Point(xb, yb)) = (Point(xa+xb,ya+yb))

bbox :: Curve -> (Point, Point)
bbox (Curve ps) = (Point (minimum (map pointX ps), minimum (map pointY ps)),
                   Point (maximum (map pointX ps), maximum (map pointY ps)))
        

{-
curve a ((as : b) ++ bs)
-}
