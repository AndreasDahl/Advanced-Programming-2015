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
rotate (Curve a as) r = Curve (fn' a) (map fn' as)
    where
        fn lcos lsin (Point (x,y)) = (point (x*lcos-y*lsin, x*lsin+y*lcos))
        fn' = (fn (cos (r/180.0*pi)) (sin (r/180.0*pi)))

translate :: Curve -> Point -> Curve
translate (Curve a as) p = Curve (fn p a) (map (fn p) as)
    where
        fn (Point(xa, ya)) (Point(xb, yb)) = (Point(xa+xb,ya+yb))

data Line = Vertical Double | Horizontal Double
    deriving (Show)

reflect :: Curve -> Line -> Curve
reflect (Curve a as) (Vertical d) = Curve (fn d a) (map (fn d) as)
    where
        fn d' (Point(x,y)) = (Point(x+2*(d'-x),y))
reflect (Curve a as) (Horizontal d) = Curve (fn d a) (map (fn d) as)
    where
        fn d' (Point(x,y)) = (Point(x,y+2*(d'-y)))

c = curve (Point(0.0,0.0)) [(Point(1.0,1.0)), (Point(2.0,2.0))]

