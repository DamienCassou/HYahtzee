{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Combination where

import Game.HYahtzee.Engine.Model
import Data.List (nub, (\\), intersect)

freeCombinations :: YTable -> [CombinationName]
freeCombinations table = 
  (fst `map` combinationTests) \\ playedCombinations table

isTableFull :: YTable -> Bool
isTableFull table = null $ freeCombinations table

{- Combination tests -}

combinationNamed :: CombinationName -> Maybe CombinationTest
combinationNamed name = lookup name combinationTests

upperCombinationNames :: [String]
upperCombinationNames = takeWhile (/= "OnePair") $ map fst combinationTests

lowerCombinationNames :: [String]
lowerCombinationNames = (map fst combinationTests) \\ upperCombinationNames


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
                   , ("Chance", hasChance)
                   ]

calculateTotalAndBonus :: YTable -> [(String, Score)]
calculateTotalAndBonus ytable = 
  let upperScores = calculateUpperScores
      lowerScores = calculateLowerScores
  in upperScores 
     ++ lowerScores 
     ++ [("Grand Total", ((snd .last) upperScores) + ((snd . last) lowerScores))]
  where
    calculateUpperScores = let combs = playedCombinations ytable `intersect` upperCombinationNames
                               totalScore = sum (map (getScoreOr0 ytable) combs)
                               bonus = if totalScore >= 63 then 35 else 0
                               total = totalScore + bonus
                               combsWithScore = [(comb, getScoreOr0 ytable comb) | comb <- combs]
                           in combsWithScore ++ [ ("Total score", totalScore)
                                                , ("Bonus", bonus)
                                                , ("Total", total) ]
    
    calculateLowerScores = let combs = playedCombinations ytable `intersect` lowerCombinationNames
                               totalScore = sum (map (getScoreOr0 ytable) combs)
                               combsWithScore = [(comb, getScoreOr0 ytable comb) | comb <- combs]
                           in combsWithScore ++ [ ("Total score", totalScore) ]


dices2List :: Dices -> [DiceVal]
dices2List (a,b,c,d,e) = [a,b,c,d,e]

hasOnePair :: CombinationTest
hasOnePair (a,b,c,d,e)
  | a==b      = CombinationResult [a,b] (2*a)
  | b==c      = CombinationResult [b,c] (2*b)
  | c==d      = CombinationResult [c,d] (2*c)
  | d==e      = CombinationResult [d,e] (2*d)
  | otherwise = CombinationResult [] 0

{- xxyyz, xyyzz, xxyzz -}
hasTwoPairs :: CombinationTest
hasTwoPairs (a,b,c,d,e)
  | a==b && c==d && b/=c =  CombinationResult [a,b,c,d] (2*a+2*c)
  | b==c && d==e && c/=d =  CombinationResult [b,c,d,e] (2*b+2*d)
  | a==b && d==e && b/=d =  CombinationResult [a,b,d,e] (2*a+2*d)
  | otherwise            =  CombinationResult [] 0

{-xxxyz xyyyz xyzzz -}
hasThreeOfAKind :: CombinationTest
hasThreeOfAKind (a,b,c,d,e)
  | a==c && c/=d = CombinationResult [a,b,c] (3*a)
  | b==d && d/=e = CombinationResult [b,c,d] (3*b)
  | c==e && b/=c = CombinationResult [c,d,e] (3*c)
  | otherwise    = CombinationResult [] 0

{- xxxxy xyyyy -}
hasFourOfAKind :: CombinationTest
hasFourOfAKind (a,b,c,d,e)
  | a==d      = CombinationResult [a,b,c,d] (4*a)
  | b==e      = CombinationResult [b,c,d,e] (4*b)
  | otherwise = CombinationResult [] 0

{- xxxyy xxyyy -}
hasFullHouse :: CombinationTest
hasFullHouse (a,b,c,d,e)
  | a==c && d==e && c/=d =  CombinationResult [a,b,c,d,e] 25
  | a==b && c==e && b/=c =  CombinationResult [a,b,c,d,e] 25
  | otherwise            =  CombinationResult [] 0
                     
hasSmallStraight :: CombinationTest
hasSmallStraight dices = let ldices = nub (dices2List dices)
                         in case ldices of
                            [1,2,3,4] ->  (CombinationResult ldices 30)
                            [2,3,4,5] ->  (CombinationResult ldices 30)
                            [3,4,5,6] ->  (CombinationResult ldices 30)
                            [1,2,3,4,5] ->  (CombinationResult (tail ldices) 30)
                            [2,3,4,5,6] ->   (CombinationResult (tail ldices) 30)
                            _ ->  (CombinationResult [] 0)
                            
hasLargeStraight :: CombinationTest
hasLargeStraight dices = let ldices = nub (dices2List dices)
                         in case ldices of
                            [1,2,3,4,5] ->  (CombinationResult ldices 40)
                            [2,3,4,5,6] ->  (CombinationResult ldices 40)
                            _ ->  (CombinationResult [] 0)

hasYahtzee :: CombinationTest
hasYahtzee (a,b,c,d,e)
  | a==e      = CombinationResult [a,b,c,d,e] 50
  | otherwise = CombinationResult [] 0

hasChance :: CombinationTest
hasChance (a,b,c,d,e) = CombinationResult [a,b,c,d,e] (a+b+c+d+e)


countXs :: DiceVal -> CombinationTest
countXs x dices = let xs = filter (== x) (dices2List dices)
                  in  (CombinationResult xs (sum xs))

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

