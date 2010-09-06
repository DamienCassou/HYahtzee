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

trAskNumOfPlayers :: Transition YData
trAskNumOfPlayers = TransNorm "trAskNumOfPlayers" id chTableFull

trSelectDices :: Transition YData
trSelectDices = TransNorm "trSelectDices" id chSelection

trChooseWhereToScore :: Transition YData
trChooseWhereToScore = TransNorm "trChooseWhereToScore" id chSwitchPlayer

trSwitchPlayer :: Transition YData
trSwitchPlayer = TransNorm "trSwitchPlayer" (nextPlayer) chTableFull

trInitialThrow :: Transition YData
trInitialThrow = TransNorm
                 "trInitialThrow" 
                 (throwDices . resetRemainingThrows . resetKeptDices)
                 chRemainThrows

trAskWantToScore :: Transition YData
trAskWantToScore = TransNorm "trAskWantToScore" id chWantToScore

trRethrow :: Transition YData
trRethrow = TransNorm "trRethrow" (throwDices . decrementRemainingThrows) chRemainThrows

trFinal :: Transition YData
trFinal = TransFinal "final" id

chSwitchPlayer :: Choice YData
chSwitchPlayer = Choice (\_ -> True) trSwitchPlayer trSwitchPlayer

chTableFull :: Choice YData
chTableFull = Choice isFull trFinal trInitialThrow

chRemainThrows :: Choice YData
chRemainThrows = Choice ((> 1) . remainingThrows) trAskWantToScore trChooseWhereToScore

chWantToScore :: Choice YData
chWantToScore = Choice wantToScore trChooseWhereToScore trSelectDices

chSelection :: Choice YData
chSelection = Choice  selectionIsOk trRethrow trSelectDices

