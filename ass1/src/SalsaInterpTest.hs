{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SalsaInterpTest where

import Test.HUnit
import Test.QuickCheck

import SalsaInterp

test1 = TestCase
    (assertEqual
        "iterpolate"
        [(1,1), (2,2)]
        (interpolate 2 (0,0) (2,2)))

main = runTestTT (TestList [TestLabel "test1" test1])

newtype SmallInts = SI Integer
    deriving (Eq, Show)

newtype Pos = Pos Position
    deriving (Eq, Show)

instance Arbitrary SmallInts where
    arbitrary = SI <$> choose (1, 60)

instance Arbitrary Pos where
    arbitrary = do
        Positive x <- arbitrary
        Positive y <- arbitrary
        return $ Pos (x, y)


prop_checkInterpolateLength (SI i) (Pos p1) (Pos p2) = length (interpolate i p1 p2) == fromIntegral i
prop_checkInterpolateEndPoint (SI i) (Pos p1) (Pos p2) = last (interpolate i p1 p2) == p2



return []
runTests = $quickCheckAll
