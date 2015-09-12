import Test.HUnit
import SalsaInterp

test1 = TestCase 
    (assertEqual 
        "iterpolate" 
        ([(1,1), (2,2)]) 
        (interpolate 2 (0,0) (2,2)))

main = runTestTT (TestList [TestLabel "test1" test1])
