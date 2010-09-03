{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Model where

import qualified Data.Map as Map

type DiceVal = Int
type Dices = (DiceVal,DiceVal,DiceVal,DiceVal,DiceVal)
type Score = Int

data CombinationResult = CombinationResult [DiceVal] Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> CombinationResult

type CombinationName = String

newtype YTable = YTable (Map.Map CombinationName Score)

makeTable:: YTable
makeTable = YTable Map.empty

getScore :: YTable -> CombinationName -> Maybe Score
getScore (YTable table) testName = Map.lookup testName table

getScoreOr0 :: YTable -> CombinationName -> Score
getScoreOr0 ytable testName = case getScore ytable testName of
                                 Just val -> val
                                 Nothing -> 0

addScore :: CombinationName -> Score -> YTable -> YTable
addScore k a (YTable table) = YTable $ Map.insert k a table

playedCombinations :: YTable -> [CombinationName]
playedCombinations (YTable table) = Map.keys table

