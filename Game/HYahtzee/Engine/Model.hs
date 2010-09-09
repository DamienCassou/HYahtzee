{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Model where

import qualified Data.Map as Map
import Data.Maybe

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
getScoreOr0 ytable testName = fromMaybe 0 $ getScore ytable testName

addScore :: CombinationName -> Score -> YTable -> YTable
addScore k a (YTable table) = YTable $ Map.insert k a table

playedCombinations :: YTable -> [CombinationName]
playedCombinations (YTable table) = Map.keys table

