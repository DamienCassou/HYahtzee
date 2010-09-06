{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Transition

import Data.List (sort)
import Control.Monad
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

displayDices :: [DiceVal] -> IO ()
displayDices dices = do putStr "{"
                        displayDices_ dices
                        putStrLn "}" where
                          displayDices_ :: [DiceVal] -> IO ()
                          displayDices_ [] = return ()
                          displayDices_ (d1:rest@(_:_)) = do putStr $ show d1 ++ ", "
                                                             displayDices_ rest
                          displayDices_ (d1:[]) = putStr $ show d1

displayTable :: YTable -> IO ()
displayTable ytable = let table = [(name, getScore ytable name) | (name,_) <- combinationTests]
                      in table `forM_` (\(name,score) -> putStrLn $ name ++ "\t\t"
                                       ++ (case score of
                                              Just s -> show s
                                              Nothing -> ""))

displayPlayerHeader :: Int -> IO ()
displayPlayerHeader num = do putStrLn   "--------"
                             putStrLn $ "Player " ++ (show num)
                             putStrLn   "--------"
                             
displayState :: YData -> IO YData
displayState ydata = do displayPlayerHeader $ ydCurPlayer ydata + 1
                        displayTable $ ydTable ydata
                        displayDices $ ydDices ydata
                        return ydata

requestChoice :: String -> [String] -> IO String
requestChoice _ (choice:[]) = return choice -- no need to choose when there is only one
requestChoice title choices =
  let prettyChoices = map -- prefix each choice by a number to be typed by the user
                      (\(f,s) -> show s ++ "- " ++ f)
                      (zip choices ([1..] :: [Integer]))
  in do putStrLn title
        prettyChoices `forM_` putStrLn
        putStrLn $ "Your choice between 1 and " ++ (show . length) choices
        input <- getLine
        case reads input of
          [(num,_)] -> if between 1 (length choices) num
                          then return $ choices !! (num - 1)
                       else requestChoice title choices
          _ -> requestChoice title choices

list2dices :: [DiceVal] -> Dices
list2dices dices = list2dices_ $ sort dices where
  list2dices_ (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
  list2dices_ _                           = (0,0,0,0,0)

isFull :: YData -> Bool
isFull = isTableFull . ydTable

request :: String -> IO Bool
request title = do putStrLn $ title ++ " [y/n] "
                   line <- getLine
                   case line of
                     "y" -> return True
                     "n" -> return False
                     _   -> request title

requestInt :: String -> IO Int
requestInt title = do putStrLn title 
                      line <- getLine
                      case (reads line)::[(Int,String)] of
                        [(val, _)] -> return val
                        _          -> requestInt title


readSequence :: (Read a) => String -> [a]
readSequence "" = []
readSequence line = let parse = map (reads . (: [])) line
                    in if any null parse
                       then []
                       else map (fst . head) parse

askNumOfPlayers :: YData -> IO YData
askNumOfPlayers ydata = do num <- requestInt "How many players?"
                           let tables = replicate num makeTable
                           return ydata {ydTables = tables, ydCurPlayer = 0, ydNumPlayers = num}

nextPlayer :: YData -> YData
nextPlayer ydata = ydata {ydCurPlayer = (ydCurPlayer ydata + 1) `mod` (ydNumPlayers ydata)}

resetRemainingThrows :: YData -> YData
resetRemainingThrows ydata = ydata {remainingThrows = maxThrows}

resetKeptDices :: YData -> YData
resetKeptDices ydata = ydata {keptDices = []}

chooseWhereToScore :: YData -> IO YData
chooseWhereToScore ydata = do let availableCombs = freeCombinations $ ydTable ydata
                              choice <- requestChoice "Where do you want to score?" availableCombs
                              case combinationNamed choice of
                                Nothing -> chooseWhereToScore ydata
                                Just test -> let dices = list2dices $ ydDices ydata
                                                 (CombinationResult _ score) = test dices
                                                 newTable = addScore choice score $ ydTable ydata
                                             in return $ changeTable newTable ydata
between :: Int -> Int -> Int -> Bool
between min_ max_ val = (min_ <= val) && (val <= max_)

decrementRemainingThrows :: YData -> YData
decrementRemainingThrows ydata = ydata {remainingThrows = remainingThrows ydata - 1}

askIfWantToScore :: YData -> IO YData
askIfWantToScore ydata = do want <- request "Do you want to score now?"
                            return ydata {wantToScore = want}
                         
askForSelection :: YData -> IO YData
askForSelection ydata = do displayDices [1..5]
                           putStrLn "Type indices of dices to keep (e.g., 145):"
                           line <- getLine
                           let indices = readSequence line
                           if all (between 1 5) indices
                             then let dices = ydDices ydata
                                      kept = [ dices !! (index - 1) | index <- indices]
                                  in return ydata {keptDices = sort kept}
                             else askForSelection ydata

confirmSelection :: YData -> IO YData
confirmSelection ydata = do displayDices $ keptDices ydata
                            keep <- request "Do you want to keep these dices?"
                            return ydata {selectionIsOk = keep}

displayCompleteTable :: Int -> YData -> IO ()
displayCompleteTable numPlayer ydata = let ytable = ydTables ydata !! numPlayer
                                           table = calculateTotalAndBonus ytable
                                       in do displayPlayerHeader (numPlayer + 1)
                                             table `forM_` (\(name, score) -> putStrLn $ name ++ "\t\t" ++ show score)

displayCompleteTables :: YData -> IO YData
displayCompleteTables ydata = do putStrLn "\n"
                                 [0..(ydNumPlayers ydata - 1)] `forM_` (\numPlayer -> displayCompleteTable numPlayer ydata >> putStrLn "")
                                 return ydata

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

iothingy :: String -> YData -> IO YData
iothingy "trSelectDices" = confirmSelection <=< askForSelection <=< displayState
iothingy "trChooseWhereToScore" = chooseWhereToScore <=< displayState
iothingy "final" = displayCompleteTables
iothingy "trInitialThrow" = return
iothingy "trAskWantToScore" = askIfWantToScore <=< displayState
iothingy "trRethrow" = return
iothingy "trAskNumOfPlayers" = askNumOfPlayers
iothingy _ = return

logicMain :: IO ()
logicMain = do ydata <- makeYData
               _ <- executeTransition ydata trAskNumOfPlayers iothingy
               return ()
                     



