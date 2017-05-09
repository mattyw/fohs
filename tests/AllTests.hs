module Main where

import Test.HUnit
import System.Exit
import Control.Monad

import Model.Model

main = do
    counts <- runTestTT allTests
    when (failures counts > 0 || errors counts > 0)
        exitFailure

allTests :: Test
allTests = "alltests" ~: [TestLabel "basic sum" testBasicSum]

testBasicSum = TestCase (do
    assertEqual "simple sum" [3] (runS "2 1 +"))
