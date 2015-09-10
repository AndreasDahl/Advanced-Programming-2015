import Test.HUnit
import Test.QuickCheck
import Curves

c1 = curve (point(0.0,0.0)) [point(1.0,1.0), point(2.0,2.0)]
c2 = curve (point(3,3)) [point(4,4)]
c3 = curve (point(0.0,0.0)) [point(1,1),point(2,2),point(3,3),point(4,4)]

-- QuickCheck tests
instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (point (x, y))

instance Arbitrary Curve where
  arbitrary = do
    p <- arbitrary
    ps <- arbitrary
    return (curve p ps)

testPointX = quickCheck ((\ p@(Point(x,y)) -> pointX p == x) :: Point -> Bool)
testPointY = quickCheck ((\ p@(Point(x,y)) -> pointY p == y) :: Point -> Bool)

checkConnect = quickCheck (\ c1@(Curve p ps) c2@(Curve p2 ps2)
  -> c1 `connect` c2 == Curve p (ps ++ (p2 : ps2)))

-- Manual Tests
testConnect = TestCase
  (assertEqual "for connect c1 c2," c3 (c1 `connect` c2))

tests = TestList [TestLabel "testConnect" testConnect]
