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


data Curve = Curve Point [Point]
    deriving (Show)

curve :: Point -> [Point] -> Curve
curve p ps = Curve p ps

connect :: Curve -> Curve -> Curve
connect (Curve a as) (Curve b bs) = curve a (as ++ (b : bs))

rotate :: Curve -> Double -> Curve
rotate (Curve a as) r = Curve (fr' a) (map fr' as)
    where
        fr lcos lsin (Point (x,y)) = (point (x*lcos-y*lsin, x*lsin+y*lcos))
        fr' = (fr (cos (r/180.0*pi)) (sin (r/180.0*pi)))

translate :: Curve -> Point -> Curve
translate (Curve a as) p = Curve (ft p a) (map (ft p) as)
    where
        ft (Point(xa, ya)) (Point(xb, yb)) = (Point(xa+xb,ya+yb))

{-
curve a ((as : b) ++ bs)
-}