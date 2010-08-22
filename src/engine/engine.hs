{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine where

import Data.List
import Test.HUnit 
import Random

type DiceVal = Int
type Dices = (DiceVal,DiceVal,DiceVal,DiceVal,DiceVal)
type Score = Int

rollDices :: Int -> [IO DiceVal]
rollDices 0 = []
rollDices x = rollDice : rollDices (x - 1) where
  rollDice = getStdRandom (randomR (1,6))

{- Combination tests -}

data CombinationResult = MakeCombinationResult [DiceVal] Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> CombinationResult

dices2List :: Dices -> [DiceVal]
dices2List (a,b,c,d,e) = [a,b,c,d,e]

hasOnePair :: CombinationTest
hasOnePair (a,b,c,d,e)
  | a==b      = MakeCombinationResult [a,b] (2*a)
  | b==c      = MakeCombinationResult [b,c] (2*b)
  | c==d      = MakeCombinationResult [c,d] (2*c)
  | d==e      = MakeCombinationResult [d,e] (2*d)
  | otherwise = MakeCombinationResult [] 0

{- xxyyz, xyyzz, xxyzz -}
hasTwoPairs :: CombinationTest
hasTwoPairs (a,b,c,d,e)
  | a==b && c==d && b/=c =  MakeCombinationResult [a,b,c,d] (2*a+2*c)
  | b==c && d==e && c/=d =  MakeCombinationResult [b,c,d,e] (2*b+2*d)
  | a==b && d==e && b/=d =  MakeCombinationResult [a,b,d,e] (2*a+2*d)
  | otherwise            =  MakeCombinationResult [] 0

{-xxxyz xyyyz xyzzz -}
hasThreeOfAKind :: CombinationTest
hasThreeOfAKind (a,b,c,d,e)
  | a==c && c/=d = MakeCombinationResult [a,b,c] (3*a)
  | b==d && d/=e = MakeCombinationResult [b,c,d] (3*b)
  | c==e && b/=c = MakeCombinationResult [c,d,e] (3*c)
  | otherwise    = MakeCombinationResult [] 0

{- xxxxy xyyyy -}
hasFourOfAKind :: CombinationTest
hasFourOfAKind (a,b,c,d,e)
  | a==d      = MakeCombinationResult [a,b,c,d] (4*a)
  | b==e      = MakeCombinationResult [b,c,d,e] (4*b)
  | otherwise = MakeCombinationResult [] 0

{- xxxyy xxyyy -}
hasFullHouse :: CombinationTest
hasFullHouse (a,b,c,d,e)
  | a==c && d==e && c/=d =  MakeCombinationResult [a,b,c,d,e] 25
  | a==b && c==e && b/=c =  MakeCombinationResult [a,b,c,d,e] 25
  | otherwise            =  MakeCombinationResult [] 0
                     
hasSmallStraight :: CombinationTest
hasSmallStraight dices = let ldices = nub (dices2List dices)
                         in case ldices of
                            [1,2,3,4] ->  (MakeCombinationResult ldices 30)
                            [2,3,4,5] ->  (MakeCombinationResult ldices 30)
                            [3,4,5,6] ->  (MakeCombinationResult ldices 30)
                            [1,2,3,4,5] ->  (MakeCombinationResult (tail ldices) 30)
                            [2,3,4,5,6] ->   (MakeCombinationResult (tail ldices) 30)
                            _ ->  (MakeCombinationResult [] 0)
                            
hasLargeStraight :: CombinationTest
hasLargeStraight dices = let ldices = nub (dices2List dices)
                         in case ldices of
                            [1,2,3,4,5] ->  (MakeCombinationResult ldices 40)
                            [2,3,4,5,6] ->  (MakeCombinationResult ldices 40)
                            _ ->  (MakeCombinationResult [] 0)

hasYahtzee :: CombinationTest
hasYahtzee (a,b,c,d,e)
  | a==e      = MakeCombinationResult [a,b,c,d,e] 50
  | otherwise = MakeCombinationResult [] 0

hasChance :: CombinationTest
hasChance (a,b,c,d,e) = MakeCombinationResult [a,b,c,d,e] (a+b+c+d+e)


countXs :: DiceVal -> CombinationTest
countXs x dices = let xs = filter (== x) (dices2List dices)
                  in  (MakeCombinationResult xs (sum xs))

has1s :: CombinationTest
has1s = countXs 1

has2s :: CombinationTest
has2s = countXs 2

has3s :: CombinationTest
has3s = countXs 3

has4s :: CombinationTest
has4s = countXs 4

has5s :: CombinationTest
has5s = countXs 5

has6s :: CombinationTest
has6s = countXs 6

{- Unit Tests -}

assertTestResult :: CombinationTest -> Dices -> [DiceVal] -> Score -> Assertion
assertTestResult providedTest providedDices expectedDices expectedScore =
  assertEqual ""
   (MakeCombinationResult expectedDices expectedScore)
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
                      has1s (1,1,1,2,3) [1,1,1] 3)
                      
tests :: Test
tests = TestList [
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
        
main :: IO Counts
main = runTestTT tests