{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Model where

import qualified Data.Map as Map

type DiceVal = Int
type Dices = (DiceVal,DiceVal,DiceVal,DiceVal,DiceVal)
type Score = Int

data CombinationResult = MakeCombinationResult [DiceVal] Score
                       deriving (Show,Eq)

type CombinationTest = Dices -> CombinationResult

type CombinationName = String

newtype YTable = YTable (Map.Map CombinationName Score)

makeTable:: YTable
makeTable = YTable Map.empty

getScore :: YTable -> CombinationName -> Maybe Score
getScore (YTable table) testName = Map.lookup testName table

addScore :: CombinationName -> Score -> YTable -> YTable
addScore k a (YTable table) = YTable $ Map.insert k a table

playedCombinations :: YTable -> [CombinationName]
playedCombinations (YTable table) = Map.keys table

