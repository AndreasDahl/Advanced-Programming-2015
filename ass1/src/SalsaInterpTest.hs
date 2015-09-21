{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SalsaInterpTest where

import Control.Monad

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

newtype ValidProgram = Valid Program
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
    arbitrary = sized exprN
        where
            exprN 0 = liftM Const arbitrary
            exprN n = oneof [liftM  Const arbitrary,
                             liftM2 Plus subExpr subExpr,
                             liftM2 Minus subExpr subExpr,
                             liftM2 Mult subExpr subExpr,
                             liftM2 Div subExpr subExpr]
                where
                    subExpr = exprN (n `div` 2)

instance Arbitrary Pos where
    arbitrary = oneof [liftM2 Abs arbitrary arbitrary,
                       liftM2 Rel arbitrary arbitrary]

instance Arbitrary Shape where
    arbitrary = oneof [rectangle, circle]
        where
            rectangle = do
                pos <- arbitrary
                (Positive w, Positive h) <- arbitrary
                c <- arbitrary
                b <- arbitrary
                return $ Rectangle pos (w,h) c b
            circle = do
                pos <- arbitrary
                Positive r <- arbitrary
                c <- arbitrary
                b <- arbitrary
                return $ Circle pos r c b

instance Arbitrary Context where
    arbitrary = do
        fr <- arbitrary
        co <- listOf content
        return $ Con fr co
        where
            content :: Gen (Ident, Shape)
            content = do
                i <- arbitrary
                s <- arbitrary
                return (i, s)

instance Arbitrary ValidProgram where
    arbitrary = Valid <$> do n <- choose(0, 30)
                             genProg n [rectGen, circGen]
        where
            genProg :: Integer -> [Gen Command] -> Gen [Command]
            genProg 0 g = do {c <- oneof g; return [c]}
            genProg n g = do
                c <- oneof g
                case c of
                    Rect i _ _ _ _ _ _ -> do
                        cs <- genProg (n-1) (moveGen i : toggleGen i : g)
                        return $ c : cs
                    Circ i _ _ _ _ _ -> do
                        cs <- genProg (n-1) (moveGen i : toggleGen i : g)
                        return $ c : cs
                    _ -> do
                        cs <- genProg (n-1) g
                        return $ c : cs
            rectGen = do
                x <- arbitrary
                i <- arbitrary
                y <- arbitrary
                w <- arbitrary
                h <- arbitrary
                c <- arbitrary
                b <- arbitrary
                return $ Rect i x y w h c b
            circGen =  do
                i <- arbitrary
                x <- arbitrary
                y <- arbitrary
                r <- arbitrary
                c <- arbitrary
                b <- arbitrary
                return $ Circ i x y r c b
            moveGen :: Ident -> Gen Command
            moveGen i = do
                p <- arbitrary
                return $ Move i p
            toggleGen :: Ident -> Gen Command
            toggleGen i = return $ Toggle i


-- Interpolate
prop_InterpolateLength (SI i) (Pos p1) (Pos p2) = length (interpolate i p1 p2) == fromIntegral i
prop_InterpolateEndPoint (SI i) (Pos p1) (Pos p2) = last (interpolate i p1 p2) == p2

prop_addShapeThenLookUp i context shape = case shapeLookup (addShape context i shape) i of
    Right _ -> True
    _       -> False


prop_runProg (SI i) (Valid prog) = case runProg i prog of
    Right _ -> True
    _       -> False

return []
runTests = $quickCheckAll
