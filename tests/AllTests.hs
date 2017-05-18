module Main where

import Test.HUnit
import Test.QuickCheck
import System.Exit
import Control.Monad

import Model.Model

main = do
    counts <- runTestTT allTests
    when (failures counts > 0 || errors counts > 0)
        exitFailure

allTests :: Test
allTests = "alltests" ~: [  testSum
                          , testMul
                          , testEq
                          , testGt
                          , testLt
                          , testAdd
                          , testDup
                          , testSub]

testSum = TestCase (do
    assertEqual "simple sum" [3] (runS "2 1 +"))

testMul = TestCase (do
    assertEqual "simple mul" [6] (runS "2 3 *"))

testSub = TestCase (do
    assertEqual "simple sub" [1] (runS "1 2 -"))

testEq = TestCase (do
    assertEqual "simple neq" [0] (runS "2 1 =")
    assertEqual "simple eq" [1] (runS "1 1 ="))

testGt = TestCase (do
    assertEqual "simple ngt" [0] (runS "2 1 >")
    assertEqual "simple gteq" [0] (runS "1 1 >")
    assertEqual "simple gt" [1] (runS "1 2 >"))

testLt = TestCase (do
    assertEqual "simple lt" [1] (runS "2 1 <")
    assertEqual "simple nlte" [0] (runS "1 1 <")
    assertEqual "simple nlte" [0] (runS "1 2 <"))

testDup = TestCase (do
    assertEqual "simple neq" [1, 1, 2] (runS "2 1 dup"))

addProperty :: Int -> Int -> Bool
addProperty a b = add [a, b] == [a+b]

-- duplicating the top of the stack and multiplying is
-- the square function
dupMul :: StackOp
dupMul = mul . dup

squareSameAsDupMul :: Int -> Bool
squareSameAsDupMul x = [x*x] == dupMul [x]

testAdd = TestCase (do
    quickCheck addProperty
    quickCheck squareSameAsDupMul 
    )
