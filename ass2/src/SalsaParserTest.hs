module SalsaParserTest where

import SimpleParse
import SalsaParser
import Test.HUnit

testValidInteger :: Test
testValidInteger = TestCase $
    assertEqual "for integerParser," [(1234567890, [])] (parseEof integerParser "1234567890")



tests :: Test
tests = TestList [TestLabel "testValidInteger" testValidInteger]
