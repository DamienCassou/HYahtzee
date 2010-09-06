{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Transition

import Data.List (sort)
import System.Random (getStdGen, randomRs)
  
data YData = YData { ydTables :: [YTable]
                   , ydCurPlayer :: Int
                   , ydNumPlayers :: Int
                   , ydRandoms :: [DiceVal]
                   , ydDices :: [DiceVal]
                   , keptDices :: [DiceVal]
                   , remainingThrows :: Int
                   , wantToScore :: Bool
                   , selectionIsOk :: Bool
                   }

maxThrows :: Int
maxThrows = 3

makeYData :: IO YData
makeYData = do gen <- getStdGen
               let randomValues = (randomRs (1,6) gen)
               return (YData [] 0 0 randomValues [] [] maxThrows True True)

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

throwDices :: YData -> YData
throwDices ydata = let (dices, newYData1) = consumeRandoms (5 - length (keptDices ydata)) ydata
                   in newYData1 {ydDices = dices ++ keptDices ydata}

ydTable :: YData -> YTable
ydTable ydata = ydTables ydata !! (ydCurPlayer ydata)

changeTable :: YTable -> YData -> YData
changeTable newTable ydata = 
  case splitAt (ydCurPlayer ydata) (ydTables ydata) of
    (before, (_:after)) -> ydata {ydTables = before ++ [newTable] ++ after}
    _                   -> ydata


list2dices :: [DiceVal] -> Dices
list2dices dices = list2dices_ $ sort dices where
  list2dices_ (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
  list2dices_ _                           = (0,0,0,0,0)

isFull :: YData -> Bool
isFull = isTableFull . ydTable

readSequence :: (Read a) => String -> [a]
readSequence "" = []
readSequence line = let parse = map (reads . (: [])) line
                    in if any null parse
                       then []
                       else map (fst . head) parse

nextPlayer :: YData -> YData
nextPlayer ydata = ydata {ydCurPlayer = (ydCurPlayer ydata + 1) `mod` (ydNumPlayers ydata)}

resetRemainingThrows :: YData -> YData
resetRemainingThrows ydata = ydata {remainingThrows = maxThrows}

resetKeptDices :: YData -> YData
resetKeptDices ydata = ydata {keptDices = []}

between :: Int -> Int -> Int -> Bool
between min_ max_ val = (min_ <= val) && (val <= max_)

decrementRemainingThrows :: YData -> YData
decrementRemainingThrows ydata = ydata {remainingThrows = remainingThrows ydata - 1}

data YLabel = SelectDices | ChooseWhereToScore | InitialThrow | AskWantToScore 
            | Rethrow | AskNumOfPlayers | SwitchPlayer | Final

trAskNumOfPlayers :: Transition YLabel YData
trAskNumOfPlayers = TransNorm AskNumOfPlayers id chTableFull

trSelectDices :: Transition YLabel YData
trSelectDices = TransNorm SelectDices id chSelection

trChooseWhereToScore :: Transition YLabel YData
trChooseWhereToScore = TransNorm ChooseWhereToScore id chSwitchPlayer

trSwitchPlayer :: Transition YLabel YData
trSwitchPlayer = TransNorm SwitchPlayer (nextPlayer) chTableFull

trInitialThrow :: Transition YLabel YData
trInitialThrow = TransNorm
                 InitialThrow
                 (throwDices . resetRemainingThrows . resetKeptDices)
                 chRemainThrows

trAskWantToScore :: Transition YLabel YData
trAskWantToScore = TransNorm AskWantToScore id chWantToScore

trRethrow :: Transition YLabel YData
trRethrow = TransNorm Rethrow (throwDices . decrementRemainingThrows) chRemainThrows

trFinal :: Transition YLabel YData
trFinal = TransFinal Final id

chSwitchPlayer :: Choice YLabel YData
chSwitchPlayer = Choice (\_ -> True) trSwitchPlayer trSwitchPlayer

chTableFull :: Choice YLabel YData
chTableFull = Choice isFull trFinal trInitialThrow

chRemainThrows :: Choice YLabel YData
chRemainThrows = Choice ((> 1) . remainingThrows) trAskWantToScore trChooseWhereToScore

chWantToScore :: Choice YLabel YData
chWantToScore = Choice wantToScore trChooseWhereToScore trSelectDices

chSelection :: Choice YLabel YData
chSelection = Choice  selectionIsOk trRethrow trSelectDices

