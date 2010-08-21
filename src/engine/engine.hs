{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine where

import Data.List
import Test.HUnit 

type DiceVal = Int
type Dices = (DiceVal,DiceVal,DiceVal,DiceVal,DiceVal)
type Score = Int

data CombinationResult = MakeCombinationResult [DiceVal] Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> Maybe CombinationResult

dices2List :: Dices -> [DiceVal]
dices2List (a,b,c,d,e) = [a,b,c,d,e]

hasOnePair :: CombinationTest
hasOnePair (a,b,c,d,e)
  | (a==b) = Just (MakeCombinationResult [a,b] (2*a))
  | (b==c) = Just (MakeCombinationResult [b,c] (2*b))
  | (c==d) = Just (MakeCombinationResult [c,d] (2*c))
  | (d==e) = Just (MakeCombinationResult [d,e] (2*d))
  | otherwise = Nothing

{- xxyyz, xyyzz, xxyzz -}
hasTwoPairs :: CombinationTest
hasTwoPairs (a,b,c,d,e)
  | (a==b && c==d && b/=c) = Just (MakeCombinationResult [a,b,c,d] (2*a+2*c))
  | (b==c && d==e && c/=d) = Just (MakeCombinationResult [b,c,d,e] (2*b+2*d))
  | (a==b && d==e && b/=d) = Just (MakeCombinationResult [a,b,d,e] (2*a+2*d))
  | otherwise              = Nothing

{-xxxyz xyyyz xyzzz -}
hasThreeOfAKind :: CombinationTest
hasThreeOfAKind (a,b,c,d,e)
  | (a==c && c/=d) = Just (MakeCombinationResult [a,b,c] (3*a))
  | (b==d && d/=e) = Just (MakeCombinationResult [b,c,d] (3*b))
  | (c==e && b/=c) = Just (MakeCombinationResult [c,d,e] (3*c))
  | otherwise              = Nothing



{- xxxxy xyyyy -}
hasFourOfAKind :: CombinationTest
hasFourOfAKind (a,b,c,d,e)
  | (a==d)    = Just (MakeCombinationResult [a,b,c,d] (4*a))
  | (b==e)    = Just (MakeCombinationResult [b,c,d,e] (4*b))
  | otherwise = Nothing

{- xxxyy xxyyy -}
hasFullHouse :: CombinationTest
hasFullHouse (a,b,c,d,e)
  | (a==c && d==e && c/=d) = Just (MakeCombinationResult [a,b,c,d,e] 25)
  | (a==b && c==e && b/=c) = Just (MakeCombinationResult [a,b,c,d,e] 25)
  | otherwise      = Nothing
                     
hasSmallStraight :: CombinationTest
hasSmallStraight dices = let ldices = nub (dices2List dices)
                         in case (length ldices) of
                            4 -> Just (MakeCombinationResult ldices 30)
                            5 -> Just (MakeCombinationResult (tail ldices) 30)
                            _ -> Nothing
                            
hasLargeStraight :: CombinationTest
hasLargeStraight dices = let ldices = nub (dices2List dices)
                         in case (length ldices) of
                            5 -> Just (MakeCombinationResult ldices 40)
                            _ -> Nothing

hasYahtzee :: CombinationTest
hasYahtzee (a,b,c,d,e)
  | (a==e)    = Just (MakeCombinationResult [a,b,c,d,e] 50)
  | otherwise = Nothing

hasChance :: CombinationTest
hasChance (a,b,c,d,e) = Just (MakeCombinationResult [a,b,c,d,e] (a+b+c+d+e))

testHasOnePair :: Test
testHasOnePair = TestCase (do
                              assertEqual ""
                                (Just (MakeCombinationResult [1,1] 2))
                                (hasOnePair (1,1,2,3,4))
                              assertEqual ""
                                (Just (MakeCombinationResult [2,2] 4))
                                (hasOnePair (1,2,2,3,4))
                              assertEqual ""
                                (Just (MakeCombinationResult [2,2] 4))
                                (hasOnePair (1,2,2,2,2)))

testHasTwoPairs :: Test
testHasTwoPairs= TestCase (do
                              assertEqual ""
                                (Just (MakeCombinationResult [1,1,2,2] 6))
                                (hasTwoPairs (1,1,2,2,4))
                              assertEqual ""
                                (Nothing)
                                (hasTwoPairs (1,1,1,1,2))
                              assertEqual ""
                                (Nothing)
                                (hasTwoPairs (1,1,1,1,1)))
                 
testHasThreeOfAKind :: Test
testHasThreeOfAKind  = TestCase (assertEqual ""
                                 (Just (MakeCombinationResult [1,1,1] 3))
                                 (hasThreeOfAKind (1,1,1,2,4)))
                       
testHasFourOfAKind :: Test
testHasFourOfAKind = TestCase (assertEqual ""
                               (Just (MakeCombinationResult [1,1,1,1] 4))
                               (hasFourOfAKind (1,1,1,1,4)))
                      
testHasFullHouse :: Test
testHasFullHouse = TestCase (do
                                assertEqual ""
                                  (Just (MakeCombinationResult [1,1,1,2,2] 25))
                                  (hasFullHouse (1,1,1,2,2))
                                assertEqual ""
                                  Nothing
                                  (hasFullHouse (1,1,1,1,1)))
                    
testHasSmallStraight :: Test
testHasSmallStraight = TestCase (do
                                    assertEqual ""
                                      (Just (MakeCombinationResult [1,2,3,4] 30))
                                      (hasSmallStraight (1,2,3,4,4))
                                    assertEqual ""
                                      (Just (MakeCombinationResult [2,3,4,5] 30))
                                      (hasSmallStraight (1,2,3,4,5)))
                        
testHasLargeStraight :: Test
testHasLargeStraight = TestCase (assertEqual ""
                                 (Just (MakeCombinationResult [1,2,3,4,5] 40))
                                 (hasLargeStraight (1,2,3,4,5)))

testHasYahtzee :: Test
testHasYahtzee = TestCase (assertEqual ""
                           (Just (MakeCombinationResult [2,2,2,2,2] 50))
                           (hasYahtzee (2,2,2,2,2)))

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
  ]
        
main :: IO Counts
main = runTestTT tests