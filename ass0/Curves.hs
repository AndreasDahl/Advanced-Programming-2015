module Curves where

import Text.Printf

{- Part 1 -}
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
    deriving (Show, Eq)

curve :: Point -> [Point] -> Curve
curve = Curve

connect :: Curve -> Curve -> Curve
connect (Curve a as) (Curve b bs) = Curve a (as ++ (b:bs))

rotate :: Curve -> Double -> Curve
rotate (Curve a as) r = Curve (fn' a) (map fn' as)
    where
        fn lcos lsin (Point (x,y)) = point (x*lcos-y*lsin, x*lsin+y*lcos)
        fn' = fn (cos (r/180.0*pi)) (sin (r/180.0*pi))

translate :: Curve -> Point -> Curve
translate (Curve (Point(xa, ya)) as) (Point(xp, yp)) =
    Curve (Point(xp, yp)) (map (fn (Point(xp-xa, yp-ya))) as)
    where
        fn (Point(xa', ya')) (Point(xb', yb')) = Point(xa'+xb', ya'+yb')

data Line = Vertical Double | Horizontal Double
    deriving (Show)

reflect :: Curve -> Line -> Curve
reflect (Curve a as) (Vertical d) = Curve (fn d a) (map (fn d) as)
    where
        fn d' (Point(x,y)) = Point(x+2*(d'-x),y)
reflect (Curve a as) (Horizontal d) = Curve (fn d a) (map (fn d) as)
    where
        fn d' (Point(x,y)) = Point(x,y+2*(d'-y))

bbox :: Curve -> (Point, Point)
bbox (Curve p ps) = (Point (minimum (map pointX (p:ps)),
                            minimum (map pointY (p:ps))),
                     Point (maximum (map pointX (p:ps)),
                            maximum (map pointY (p:ps))))

width :: Curve -> Double
width c = abs(pointX pa - pointX pb)
    where (pa, pb) = bbox c

height :: Curve -> Double
height c = abs(pointY pa - pointY pb)
    where (pa, pb) = bbox c

toList :: Curve -> [Point]
toList (Curve p ps) = p:ps

{- Part 2 -}
toSVG :: Curve -> String
toSVG c = printf (
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%.0fpx\" " ++
        "height=\"%.0fpx\" version=\"1.1\"><g>\n") (width c) (height c) ++
          toSVG' c ++ "</g></svg>"
    where
        fn :: (Point, Point) -> String
        fn (Point(xa, ya), Point(xb, yb)) = printf
            ("<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n" ++
             "x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\" />\n") xa xb ya yb
        toSVG' (Curve x (y:ps)) = fn (x, y) ++ toSVG'(Curve y ps)
        toSVG' _ = ""

toFile :: Curve -> FilePath -> IO ()
toFile c f = writeFile f (toSVG c)

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)

