module SalsaParserTest where

import Test.HUnit

import SalsaAst
import SimpleParse
import SalsaParser

-- integerParser
testValidInteger = TestCase $
    assertEqual "for integerParser," [(1234567890, [])] (parseEof integerParser "1234567890")

-- identParser
testValidIdent = TestCase $
    assertEqual "for identParser \"abc_d\"," [("abc_d", [])] (parseEof identParser "abc_d")
testValidIdentWithOnlyDigits = TestCase $
    assertEqual "for identParser \"123\"," [] (parseEof identParser "123")
testValidIdentWithDigits = TestCase $
    assertEqual "for identParser \"a123\"," [("a123", [])] (parseEof identParser "a123")


testInvalidIdentDash = TestCase $
    assertEqual "for identParser \"a-b\"," [] $ parseEof identParser "a-b"

testInvalidIdentRectangle = TestCase $
    assertEqual "for identParser \"rectangle\"," [] $ parseEof identParser "rectangle"
testInvalidIdentCircle = TestCase $
    assertEqual "for identParser \"circle\"," [] $ parseEof identParser "circle"
testInvalidIdentHidden = TestCase $
    assertEqual "for identParser \"hidden\"," [] $ parseEof identParser "hidden"
testInvalidIdentToggle = TestCase $
    assertEqual "for identParser \"toggle\"," [] $ parseEof identParser "toggle"

testInvalidIdentBlue = TestCase $
    assertEqual "for identParser \"blue\"," [] $ parseEof identParser "blue"
testInvalidIdentPlum = TestCase $
    assertEqual "for identParser \"plum\"," [] $ parseEof identParser "plum"
testInvalidIdentRed = TestCase $
    assertEqual "for identParser \"red\"," [] $ parseEof identParser "red"
testInvalidIdentGreen = TestCase $
    assertEqual "for identParser \"green\"," [] $ parseEof identParser "green"
testInvalidIdentOrange = TestCase $
    assertEqual "for identParser \"orange\"," [] $ parseEof identParser "orange"

-- colourParser
testParseBlue = TestCase $
    assertEqual "for colourParser \"blue\"," [(Blue, [])] $ parseEof colourParser "blue"
testParsePlum = TestCase $
    assertEqual "for colourParser \"plum\"," [(Plum, [])] $ parseEof colourParser "plum"
testParseRed = TestCase $
    assertEqual "for colourParser \"red\"," [(Red, [])] $ parseEof colourParser "red"
testParseGreen = TestCase $
    assertEqual "for colourParser \"green\"," [(Green, [])] $ parseEof colourParser "green"
testParseOrange = TestCase $
    assertEqual "for colourParser \"orange\"," [(Orange, [])] $ parseEof colourParser "orange"

testParseBadBlue = TestCase $
    assertEqual "for colourParser \"Blue\"," [] $ parseEof colourParser "Blue"

tests = TestList [
    TestLabel "testValidInteger" testValidInteger,
    TestLabel "testValidIdentParser" testValidIdent,
    TestLabel "testValidIdentWithOnlyDigits" testValidIdentWithOnlyDigits,
    TestLabel "testValidIdentWithDigits" testValidIdentWithDigits,
    TestLabel "testInvalidIdentDash" testInvalidIdentDash,
    TestLabel "testInvalidIdentRectangle" testInvalidIdentRectangle,
    TestLabel "testInvalidIdentCircle" testInvalidIdentCircle,
    TestLabel "testInvalidIdentHidden" testInvalidIdentHidden,
    TestLabel "testInvalidIdentToggle" testInvalidIdentToggle,
    TestLabel "testInvalidIdentBlue" testInvalidIdentBlue,
    TestLabel "testInvalidIdentPlum" testInvalidIdentPlum,
    TestLabel "testInvalidIdentRed" testInvalidIdentRed,
    TestLabel "testInvalidIdentGreen" testInvalidIdentGreen,
    TestLabel "testInvalidIdentOrange" testInvalidIdentOrange,
    testParseBlue,
    testParsePlum,
    testParseRed,
    testParseGreen,
    testParseOrange,
    testParseBadBlue]


