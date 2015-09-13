import SalsaInterp
import Test.HUnit

simpleInterpolateTest = TestCase
    (assertEqual "for interpolate 3 (0,0) (3,3)"
        [(1,1),(2,2),(3,3)] (interpolate 3 (0,0) (3,3)))

tests = TestList [TestLabel "simpleInterpolateTest" simpleInterpolateTest]
