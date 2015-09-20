{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SalsaInterpTest where

import Test.HUnit
import Test.QuickCheck

import SalsaAst
import SalsaInterp

-- Unit Tests

test1 = TestCase
    (assertEqual
        "iterpolate"
        [(1,1), (2,2)]
        (interpolate 2 (0,0) (2,2)))

main = runTestTT (TestList [TestLabel "test1" test1])

-- QuickCheck

newtype SmallInts = SI Integer
    deriving (Eq, Show)

newtype Posi = Pos Position
    deriving (Eq, Show)

instance Arbitrary SmallInts where
    arbitrary = SI <$> choose (1, 60)

instance Arbitrary Posi where
    arbitrary = do
        Positive x <- arbitrary
        Positive y <- arbitrary
        return $ Pos (x, y)

instance Arbitrary Colour where
    arbitrary = elements [Blue, Plum, Red, Green, Orange]

instance Arbitrary Expr where
    arbitrary = do
        Small i <- arbitrary
        return $ Const i

absPosGen = do
    x <- arbitrary
    y <- arbitrary
    return $ Abs x y

relPosGen = do
    x <- arbitrary
    y <- arbitrary
    return $ Rel x y


instance Arbitrary Pos where
    arbitrary = oneof [absPosGen, relPosGen]

rectGen = do
    i <- arbitrary
    x <- arbitrary
    y <- arbitrary
    w <- arbitrary
    h <- arbitrary
    c <- arbitrary
    b <- arbitrary
    return $ Rect i x y w h c b

circleGen =  do
    i <- arbitrary
    x <- arbitrary
    y <- arbitrary
    r <- arbitrary
    c <- arbitrary
    b <- arbitrary
    return $ Circ i x y r c b

moveGen = do
    i <- arbitrary
    p <- arbitrary
    return $ Move i p

toggleGen = do
    i <- arbitrary
    return $ Toggle i

instance Arbitrary Command where
    arbitrary = oneof [rectGen, circleGen]


-- Interpolate
prop_InterpolateLength (SI i) (Pos p1) (Pos p2) = length (interpolate i p1 p2) == fromIntegral i
prop_InterpolateEndPoint (SI i) (Pos p1) (Pos p2) = last (interpolate i p1 p2) == p2

prop_runProg (SI i) prog = case runProg i prog of
    Right _ -> True
    _       -> False

return []
runTests = $quickCheckAll
