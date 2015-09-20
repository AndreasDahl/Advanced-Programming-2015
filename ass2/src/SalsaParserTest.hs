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
    assertEqual "for primParser \" 42\"," [(Const 42, [])] $ parseEof primParser " 42"
testPrimParseNegInt = TestCase $
    assertEqual "for primParser \"-42\"," [] $ parseEof primParser "-42"
testPrimParseXproj = TestCase $
    assertEqual "for primParser \" test . x\"," [(Xproj "test", [])] $ parseEof primParser " test . x"
testPrimParseYproj = TestCase $
    assertEqual "for primParser \" test . y\"," [(Yproj "test", [])] $ parseEof primParser " test . y"
testPrimParseElse1proj = TestCase $
    assertEqual "for primParser \" test . xy\"," [] $ parseEof primParser " test . xy"
testPrimParseElse2proj = TestCase $
    assertEqual "for primParser \" test . z\"," [] $ parseEof primParser " test . z"

-- exprParser
testSimpleExpr = TestCase $
    assertEqual "for exprParser \"1 * 2\"," [(Mult (Const 1) (Const 2), [])] $
    parseEof exprParser "1 * 2"

testExprAssociation = TestCase $
    assertEqual "for exprParser \"1 - 2 - 3\"," [(Minus (Minus (Const 1) (Const 2)) (Const 3), "")] $
    parseEof exprParser "1 - 2 - 3"

testExprPrecedence = TestCase $
    assertEqual "for exprParser \"1 + 2 * 3\"," [(Plus (Const 1) (Mult (Const 2) (Const 3)), "")] $
    parseEof exprParser "1 + 2 * 3"

-- posParser
testSimpleAbsPos = TestCase $
    assertEqual "for posParser \"(1,2)\"," [(Abs (Const 1) (Const 2), "")] $
    parseEof posParser "(1,2)"

testSimpleRelPos = TestCase $
    assertEqual "for posParser \"+(1,2)\"," [(Rel (Const 1) (Const 2), "")] $
    parseEof posParser "+(1,2)"

-- shapeDefParser
testSimpleVisibleRectDef = TestCase $
    assertEqual "for (shapeDefParser True) \"rectangle a 1 2 3 4 red\","
    [(Rect "a" (Const 1) (Const 2) (Const 3) (Const 4) Red True,"")] $
    parseEof (shapeDefParser True) "rectangle a 1 2 3 4 red"
testSimpleVisibleCircDef = TestCase $
    assertEqual "for (shapeDefParser True) \"circle a 1 2 3 red\","
    [(Circ "a" (Const 1) (Const 2) (Const 3) Red True,"")] $
    parseEof (shapeDefParser True) "circle a 1 2 3 red"
testSimpleHiddenRectDef = TestCase $
    assertEqual "for (shapeDefParser False) \"rectangle a 1 2 3 4 red\","
    [(Rect "a" (Const 1) (Const 2) (Const 3) (Const 4) Red False,"")] $
    parseEof (shapeDefParser False) "rectangle a 1 2 3 4 red"
testSimpleHiddenCircDef = TestCase $
    assertEqual "for (shapeDefParser False) \"circle a 1 2 3 red\","
    [(Circ "a" (Const 1) (Const 2) (Const 3) Red False,"")] $
    parseEof (shapeDefParser False) "circle a 1 2 3 red"

-- commandParser
testHiddenShapeCommand = TestCase $
    assertEqual "for commandParser \"hidden rectangle a 1 2 3 4 red\","
    [(Rect "a" (Const 1) (Const 2) (Const 3) (Const 4) Red False, "")] $
    parseEof commandParser "hidden rectangle a 1 2 3 4 red"

testHiddenShapeNoSpace = TestCase $
    assertEqual "for commandParser \"hiddenrectangle a 1 2 3 4 red\"," [] $
    parseEof commandParser "hiddenrectangle a 1 2 3 4 red"

testToggleCommand = TestCase $
    assertEqual "for commandParser \"toggle foo\"," [(Toggle "foo", "")] $
    parseEof commandParser "toggle foo"

testParallelCommands = TestCase $
    assertEqual "for commandParser \"toggle foo || toggle bar\","
    [(Par (Toggle "foo") (Toggle "bar"), "")] $
    parseEof commandParser "toggle foo || toggle bar"

testMoveMultipleCommand = TestCase $
    assertEqual "for commandParser \"foo bar -> (1,2)\","
    [(Par (Move "foo" (Abs (Const 1) (Const 2))) (Move "bar" (Abs (Const 1) (Const 2))), "")] $
    parseEof commandParser "foo bar -> (1,2)"

-- commandsParser
testMultipleCommands = TestCase $
    assertEqual "for commandsParser \"toggle foo toggle bar\","
    [([Toggle "foo", Toggle "bar"], "")] $
    parseEof commandsParser "toggle foo toggle bar"

testMultipleCommandsNoSpace = TestCase $
    assertEqual "for commandsParser \"rectangle foo 1 2 3 4 greenfoo -> (5,6)\","
    [] $
    parseEof commandsParser "rectangle foo 1 2 3 4 greenfoo -> (5,6)"

-- parseString
testSimpleParseString = TestCase $
    assertEqual "for parseString \"toggle foo\","
    (Right [Toggle "foo"]) $
    parseString "toggle foo"

testInvalidParseString = TestCase $
    assertEqual "for parseString \"toogle foo\","
    (Left "Fail") $
    parseString "Fail"

testParseStringTrailingSpaces = TestCase $
    assertEqual "for parseString \"toogle foo  \n\","
    (Right [Toggle "foo"]) $
    parseString "toggle foo  \n"


tests = TestList [
    testMultipleCommandsNoSpace,
    testParseStringTrailingSpaces,
    testSimpleParseString,
    testInvalidParseString,
    testMultipleCommands,
    testHiddenShapeCommand,
    testHiddenShapeNoSpace,
    testToggleCommand,
    testParallelCommands,
    testMoveMultipleCommand,
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
    testPrimParseElse2proj,
    testSimpleExpr,
    testExprAssociation,
    testExprPrecedence,
    testSimpleAbsPos,
    testSimpleRelPos,
    testSimpleVisibleRectDef,
    testSimpleVisibleCircDef,
    testSimpleHiddenRectDef,
    testSimpleHiddenCircDef]
