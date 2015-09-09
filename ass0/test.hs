module Test where

import Curves

c1 = curve (point(0.0,0.0)) [(point(1.0,1.0)), (point(2.0,2.0))]
c2 = curve (point(3,3)) [(point(4,4))]
c3 = (curve (point(0.0,0.0)) [point(1,1),point(2,2),point(3,3),point(4,4)])

testConnect :: (Curve, Curve) -> Curve -> Bool
testConnect (c1, c2) c3 = (c1 `connect` c2) == c3
