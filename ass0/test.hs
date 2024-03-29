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

-- Tests have also been run with a Horizontal variant of this
instance Arbitrary Line where
    arbitrary = do
    n <- arbitrary
    return (Vertical n)

testPointX = quickCheck ((\ p@(Point(x,y)) -> pointX p == x) :: Point -> Bool)
testPointY = quickCheck ((\ p@(Point(x,y)) -> pointY p == y) :: Point -> Bool)

checkConnect = quickCheck (\ c1@(Curve p ps) c2@(Curve p2 ps2)
  -> c1 `connect` c2 == Curve p (ps ++ (p2 : ps2)))

checkRotate = quickCheck (\ c a -> (c `rotate` a `rotate` (-a)) == c)

checkTranslateToStartingPointDoesNothing = quickCheck
    (\ c@(Curve p ps) -> translate c p == c)
checkTranslateSetsProperStart = quickCheck
    (\ c p -> let p2 = case translate c p of (Curve p2 _) -> p in p2 == p)

checkReflect = quickCheck (\ c l -> (c `reflect` l `reflect` l) == c)


-- Manual Tests
testConnect = TestCase
  (assertEqual "for connect c1 c2," c3 (c1 `connect` c2))

tests = TestList [TestLabel "testConnect" testConnect]
