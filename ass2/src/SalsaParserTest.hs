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

tests = TestList [
    TestLabel "testValidInteger" testValidInteger,
    TestLabel "testValidIdentParser" testValidIdent,
    TestLabel "testInvalidIdentDash" testInvalidIdentDash,
    TestLabel "testInvalidIdentRectangle" testInvalidIdentRectangle,
    TestLabel "testInvalidIdentCircle" testInvalidIdentCircle,
    TestLabel "testInvalidIdentHidden" testInvalidIdentHidden,
    TestLabel "testInvalidIdentToggle" testInvalidIdentToggle]
