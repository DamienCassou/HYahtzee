{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Combination where

import Game.HYahtzee.Engine.Model
import Data.List (nub, (\\))

freeCombinations :: YTable -> [CombinationName]
freeCombinations table = 
  (fst `map` combinationTests) \\ playedCombinations table

isTableFull :: YTable -> Bool
isTableFull table = null $ freeCombinations table

{- Combination tests -}

combinationTests :: [(CombinationName, CombinationTest)]
combinationTests = [ ("Aces", hasAces)
                   , ("Twos", hasTwos)
                   , ("Threes", hasThrees)
                   , ("Fours", hasFours)
                   , ("Fives", hasFives)
                   , ("Sixes", hasSixes)
                   , ("OnePair", hasOnePair)
                   , ("TwoPairs", hasTwoPairs)
                   , ("ThreeOfAKind", hasThreeOfAKind)
                   , ("FourOfAKind", hasFourOfAKind)
                   , ("FullHouse", hasFullHouse)
                   , ("SmallStraight", hasSmallStraight)
                   , ("LargeStraight", hasLargeStraight)
                   , ("Yahtzee", hasYahtzee)
                   , ("Chance", hasChance)]

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

hasAces :: CombinationTest
hasAces = countXs 1

hasTwos :: CombinationTest
hasTwos = countXs 2

hasThrees :: CombinationTest
hasThrees = countXs 3

hasFours :: CombinationTest
hasFours = countXs 4

hasFives :: CombinationTest
hasFives = countXs 5

hasSixes :: CombinationTest
hasSixes = countXs 6

