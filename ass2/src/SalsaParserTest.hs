module SalsaParserTest where

import SimpleParse
import SalsaParser
import Test.HUnit

-- integerParser
testValidInteger = TestCase $
    assertEqual "for integerParser," [(1234567890, [])] (parseEof integerParser "1234567890")

-- identParser
testValidIdent = TestCase $
    assertEqual "for identParser \"abc_d\"," [("abc_d", [])] (parseEof identParser "abc_d")
testValidIdentWithDigits = TestCase $
    assertEqual "for identParser \"123\"," [("123", [])] (parseEof identParser "123")


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

tests = TestList [
    TestLabel "testValidInteger" testValidInteger,
    TestLabel "testValidIdentParser" testValidIdent,
    TestLabel "testValidIdentParserWithDigits" testValidIdentWithDigits,
    TestLabel "testInvalidIdentDash" testInvalidIdentDash,
    TestLabel "testInvalidIdentRectangle" testInvalidIdentRectangle,
    TestLabel "testInvalidIdentCircle" testInvalidIdentCircle,
    TestLabel "testInvalidIdentHidden" testInvalidIdentHidden,
    TestLabel "testInvalidIdentToggle" testInvalidIdentToggle,
    TestLabel "testInvalidIdentBlue" testInvalidIdentBlue,
    TestLabel "testInvalidIdentPlum" testInvalidIdentPlum,
    TestLabel "testInvalidIdentRed" testInvalidIdentRed,
    TestLabel "testInvalidIdentGreen" testInvalidIdentGreen,
    TestLabel "testInvalidIdentOrange" testInvalidIdentOrange]
