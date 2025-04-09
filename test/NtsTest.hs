module Main where

import Parser (matchFlag, splitEqArgs)
import qualified System.Exit as Exit
import Test.HUnit

testSplitArgs :: Test
testSplitArgs =
  TestCase
    (assertEqual "args should be split properly" expectation actual)
  where
    expectation = ["hello", "world", "another", "key", "value"]
    actual = splitEqArgs ["hello=world", "another", "key=value"]

testFlagMatching :: Test
testFlagMatching =
  TestCase
    (assertEqual "flags should be matched with - and --" expectation actual)
  where
    expectation = [True, True, False]
    actual = matcher "-flag" : matcher "--flag" : [matcher "flag"]
    matcher = matchFlag "flag"

tests :: Test
tests =
  TestList
    [ TestLabel "arg splitting" testSplitArgs,
      TestLabel "flag matching" testFlagMatching
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
