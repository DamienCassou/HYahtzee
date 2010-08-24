{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine where

import Data.List (nub, (\\))
import Test.HUnit 
import Test.QuickCheck.Gen (Gen, choose, unGen)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

type DiceVal = Int
type Dices = (DiceVal,DiceVal,DiceVal,DiceVal,DiceVal)
type Score = Int

data CombinationResult = MakeCombinationResult [DiceVal] Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> CombinationResult

rollDices :: Int -> Gen [DiceVal]
rollDices 0 = do return []
rollDices x = do dices <- rollDices (x-1)
                 dice <- choose (1,6)
                 return (dice:dices)
  
{- Player table -}

type CombinationName = String
type YTable = Map CombinationName Score
                     
getScore :: YTable -> CombinationName -> Maybe Score
getScore table testName = Map.lookup testName table

addScore :: CombinationName -> Score -> YTable -> YTable
addScore = Map.insert

freeCombinations :: YTable -> [CombinationName]
freeCombinations table = (fst `map` combinationTests) \\ Map.keys table

isTableFull :: YTable -> Bool
isTableFull table = null $ freeCombinations table

displayDice :: Gen DiceVal -> IO ()
displayDice dice = do gen <- getStdGen
                      let (gen1, gen2) = split gen
                          val = unGen dice gen1 1
                      putStr $ show val
                      setStdGen gen2

privDisplayDices :: Gen [DiceVal] -> IO ()
privDisplayDices gdices =
  do gen1 <- getStdGen
     let dices = unGen gdices gen1 1
     case dices of
       (d1:rest@(_:_)) -> do displayDice (return d1)
                             putStr ", "
                             privDisplayDices (return rest)
       (d1:[])         -> do displayDice (return d1)
       _               -> return ()
     
displayDices :: Gen [DiceVal] -> IO ()
displayDices dices = do putStr "{"
                        privDisplayDices dices 
                        putStrLn "}"

oneThrow :: YTable -> IO (Gen [DiceVal])
oneThrow _ = let dices = rollDices 5
             in do displayDices dices >> return dices

oneTurn :: YTable -> IO YTable
oneTurn table = do oneThrow table >> return table
  

onePlayer :: IO YTable -> IO YTable
onePlayer table = do mytable <- table
                     -- let full = isTableFull mytable
                     -- if full
                     --   then table
                     --   else 
                     -- onePlayer $ 
                     oneTurn mytable

mainOnePlayer :: IO YTable
mainOnePlayer = do onePlayer (return Map.empty :: IO YTable)
                   
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
                      hasAces (1,1,1,2,3) [1,1,1] 3)
                      
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
        
mainTests :: IO Counts
mainTests = runTestTT tests