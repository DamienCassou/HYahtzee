{-# OPTIONS -Wall #-}

module Game.HYahtzee.Test.EngineTest where

import Test.HUnit 
import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Logic

{- Unit Tests -}

myTable :: YTable
myTable = ((addScore "Aces" 70) . (addScore "OnePair" 10)) makeTable

myData :: YData
myData = YData myTable [6,5,4,3,2,1] [] [] maxThrows True True

assertTestResult :: CombinationTest -> Dices -> [DiceVal] -> Score -> Assertion
assertTestResult providedTest providedDices expectedDices expectedScore =
  assertEqual ""
   (CombinationResult expectedDices expectedScore)
   (providedTest providedDices)

testHasOnePair :: Test
testHasOnePair = TestCase (do
                              assertTestResult
                                hasOnePair (1,1,2,3,4) [1,1] 2
                              assertTestResult
                                hasOnePair (1,2,2,3,4) [2,2] 4
                              assertTestResult
                                hasOnePair (1,2,2,2,2) [2,2] 4)
                 
testHasTwoPairs :: Test
testHasTwoPairs= TestCase (do
                              assertTestResult
                                hasTwoPairs (1,1,2,2,4) [1,1,2,2] 6
                              assertTestResult
                                hasTwoPairs (1,1,1,1,2) [] 0
                              assertTestResult 
                                hasTwoPairs (1,1,1,1,1) [] 0)
                 
testHasThreeOfAKind :: Test
testHasThreeOfAKind  = TestCase (assertTestResult
                                 hasThreeOfAKind (1,1,1,2,4) [1,1,1] 3)
                       
testHasFourOfAKind :: Test
testHasFourOfAKind = TestCase (assertTestResult
                               hasFourOfAKind (1,1,1,1,4) [1,1,1,1] 4)
                               
testHasFullHouse :: Test
testHasFullHouse = TestCase (do
                                assertTestResult
                                  hasFullHouse (1,1,1,2,2) [1,1,1,2,2] 25
                                assertTestResult
                                  hasFullHouse (1,1,1,1,1) [] 0)
                    
testHasSmallStraight :: Test
testHasSmallStraight = TestCase (do
                                    assertTestResult
                                      hasSmallStraight (1,2,3,4,4) [1,2,3,4] 30
                                    assertTestResult
                                      hasSmallStraight (1,2,3,4,5) [2,3,4,5] 30
                                    assertTestResult
                                      hasSmallStraight (1,2,4,5,6) [] 0)
                       
testHasLargeStraight :: Test
testHasLargeStraight = TestCase (do
                                    assertTestResult
                                      hasLargeStraight (1,2,3,4,5) [1,2,3,4,5] 40
                                    assertTestResult
                                      hasLargeStraight (1,2,3,5,6) [] 0)
                       
testHasYahtzee :: Test
testHasYahtzee = TestCase (assertTestResult
                           hasYahtzee (2,2,2,2,2) [2,2,2,2,2] 50)
                           
testHas1s :: Test
testHas1s = TestCase (assertTestResult
                      hasAces (1,1,1,2,3) [1,1,1] 3)
                      
combTests :: Test
combTests = TestList [
  TestLabel "testHasOnePair" testHasOnePair
  , TestLabel "testHasTwoPairs" testHasTwoPairs
  , TestLabel "testHasThreeOfAKind" testHasThreeOfAKind
  , TestLabel "testHasFourOfAKind" testHasFourOfAKind
  , TestLabel "testHasFullHouse" testHasFullHouse
  , TestLabel "testHasSmallStraight" testHasSmallStraight
  , TestLabel "testHasLargeStraight" testHasLargeStraight
  , TestLabel "testHasYahtzee" testHasYahtzee
  , TestLabel "testHas1s" testHas1s
  ]
                   
allTests :: Test
allTests = TestList [combTests]

mainTests :: IO Counts
mainTests = runTestTT allTests
