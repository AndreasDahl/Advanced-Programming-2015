module Curves where

import Text.Printf

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
rotate (Curve as) r = Curve (map fn' as)
    where
        fn lcos lsin (Point (x,y)) = point (x*lcos-y*lsin, x*lsin+y*lcos)
        fn' = fn (cos (r/180.0*pi)) (sin (r/180.0*pi))

translate :: Curve -> Point -> Curve
translate (Curve as) p = Curve (map (fn p) as)
    where
        fn (Point(xa, ya)) (Point(xb, yb)) = Point(xa+xb,ya+yb)

data Line = Vertical Double | Horizontal Double
    deriving (Show)

reflect :: Curve -> Line -> Curve
reflect (Curve as) (Vertical d) = Curve (map (fn d) as)
    where
        fn d' (Point(x,y)) = Point(x+2*(d'-x),y)
reflect (Curve as) (Horizontal d) = Curve (map (fn d) as)
    where
        fn d' (Point(x,y)) = Point(x,y+2*(d'-y))

bbox :: Curve -> (Point, Point)
bbox (Curve ps) = (Point (minimum (map pointX ps), minimum (map pointY ps)),
                   Point (maximum (map pointX ps), maximum (map pointY ps)))

width :: Curve -> Double
width c = abs((pointX pa) - (pointX pb))
    where (pa, pb) = (bbox c)

height :: Curve -> Double
height c = abs((pointY pa) - (pointY pb))
    where (pa, pb) = (bbox c)

toList :: Curve -> [Point]
toList (Curve ps) = ps


cur = curve (Point(0.0,0.0)) [(Point(1.0,1.0)), (Point(2.0,2.0))]

toSVG :: Curve -> String
toSVG c = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10px\" height=\"10px\" version=\"1.1\"><g>\n" ++
          toSVG' c ++ "</g></svg>"
    where
        fn :: (Point, Point) -> String
        fn ((Point(xa, ya)), (Point(xb, yb))) = printf
            ("<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
             "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />\n") xa xb ya yb
        toSVG' (Curve (x:(y:ps))) = fn (x, y) ++ toSVG'(Curve(y:ps))
        toSVG' _ = ""

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c)

