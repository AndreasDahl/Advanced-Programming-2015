{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module SalsaParserTest where

import Test.HUnit

import SalsaAst
import SimpleParse
import SalsaParser

-- integerParser
testValidInteger = TestCase $
    assertEqual "for integerParser \"1234567890\"," [(1234567890, [])] (parseEof integerParser "1234567890")

testValidIntegerWithSpace = TestCase $
    assertEqual "for integerParser \" 123\"," [(123, [])] $
    parseEof integerParser " 123"

testInvalidInteger = TestCase $
    assertEqual "for integerParser \"ab123\"," [] $
    parseEof integerParser "ab123"

testInvalidNegativeInteger = TestCase $
    assertEqual "for integerParser \"-123\"," [] $
    parseEof integerParser "-123"

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

-- identsParser
testIdentsParserSpace = TestCase $
    assertEqual "for identsParser \"foo bar\"," [(["foo", "bar"], [])] $
    parseEof identsParser "foo bar"


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

testParseColourWithSpace = TestCase $
    assertEqual "for colourParser \" blue\"," [(Blue, [])] $
    parseEof colourParser " blue"

-- primParser
testPrimParseInt = TestCase $
    assertEqual "for primParser \"int\"," [(Const 42, [])] $ parseEof primParser " 42"
testPrimParseNegInt = TestCase $
    assertEqual "for primParser \"int\"," [] $ parseEof primParser "-42"
testPrimParseXproj = TestCase $
    assertEqual "for primParser \"int\"," [(Xproj "test", [])] $ parseEof primParser " test . x"
testPrimParseYproj = TestCase $
    assertEqual "for primParser \"int\"," [(Yproj "test", [])] $ parseEof primParser " test . y"
testPrimParseElse1proj = TestCase $
    assertEqual "for primParser \"int\"," [] $ parseEof primParser " test . xy"
testPrimParseElse2proj = TestCase $
    assertEqual "for primParser \"int\"," [] $ parseEof primParser " test . z"

tests = TestList [
    TestLabel "testValidInteger" testValidInteger,
    testValidIntegerWithSpace,
    testInvalidInteger,
    testInvalidNegativeInteger,
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
    testIdentsParserSpace,
    testParseBlue,
    testParsePlum,
    testParseRed,
    testParseGreen,
    testParseOrange,
    testParseBadBlue,
    testParseColourWithSpace,
    testPrimParseInt,
    testPrimParseNegInt,
    testPrimParseXproj,
    testPrimParseYproj,
    testPrimParseElse1proj,
    testPrimParseElse2proj]


