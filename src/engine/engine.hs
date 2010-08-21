{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine where

import Test.HUnit 

type DiceVal = Int
type Dices = [DiceVal]
type Score = Int

data CombinationResult = MakeCombinationResult Dices Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> Maybe CombinationResult

hasOnePair :: CombinationTest
hasOnePair (f:(s:r)) = if (f == s)
                       then Just (MakeCombinationResult [f,s] (f+s))
                       else hasOnePair (s:r)
hasOnePair _ = Nothing

{- xxyyz, xyyzz, xxyzz-}
hasTwoPairs :: CombinationTest
hasTwoPairs (a:(b:(c:(d:(e:_)))))
  | (a==b && c==d && b/=c) = Just (MakeCombinationResult [a,b,c,d] (a+b+c+d))
  | (b==c && d==e && c/=d) = Just (MakeCombinationResult [b,c,d,e] (b+c+d+e))
  | (a==b && d==e && b/=d) = Just (MakeCombinationResult [a,b,d,e] (a+b+d+e))
  | otherwise                      = Nothing
hasTwoPairs _ = Nothing

{-xxxyz xyyyz xyzzz -}
hasThreeOfAKind :: CombinationTest
hasThreeOfAKind (a:(b:(c:(d:(e:_)))))
  | (a==b && b==c && c/=d) = Just (MakeCombinationResult [a,b,c] (3*a))
  | (b==c && c==d && d/=e) = Just (MakeCombinationResult [b,c,d] (3*b))
  | (c==d && d==e && b/=c) = Just (MakeCombinationResult [c,d,e] (3*c))
  | otherwise              = Nothing
hasThreeOfAKind _ = Nothing

testHasOnePair :: Test
testHasOnePair = TestCase (assertEqual ""
                           (Just (MakeCombinationResult [1,1] 2))
                           (hasOnePair [1,1,2,3,4]))

testHasTwoPairs :: Test
testHasTwoPairs= TestCase (assertEqual ""
                           (Just (MakeCombinationResult [1,1,2,2] 6))
                           (hasTwoPairs [1,1,2,2,4]))
                 
testHasThreeOfAKind :: Test
testHasThreeOfAKind  = TestCase (assertEqual ""
                                 (Just (MakeCombinationResult [1,1,1] 3))
                                 (hasThreeOfAKind [1,1,1,2,4]))

tests :: Test
tests = TestList [
  TestLabel "testHasOnePair" testHasOnePair
  , TestLabel "testHasTwoPairs" testHasTwoPairs
  , TestLabel "testHasThreeOfAKind" testHasThreeOfAKind
  ]
        
main :: IO Counts
main = runTestTT tests