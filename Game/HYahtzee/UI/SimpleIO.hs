{-# OPTIONS -Wall #-}

module Game.HYahtzee.UI.SimpleIO where

import Game.HYahtzee.Engine.Logic
import Control.Monad
import Data.List (sort)
import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Transition

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

askNumOfPlayers :: YData -> IO YData
askNumOfPlayers ydata = do num <- requestInt "How many players?"
                           let tables = replicate num makeTable
                           return ydata {ydTables = tables, ydCurPlayer = 0, ydNumPlayers = num}


chooseWhereToScore :: YData -> IO YData
chooseWhereToScore ydata = do let availableCombs = freeCombinations $ ydTable ydata
                              choice <- requestChoice "Where do you want to score?" availableCombs
                              case combinationNamed choice of
                                Nothing -> chooseWhereToScore ydata
                                Just test -> let dices = list2dices $ ydDices ydata
                                                 (CombinationResult _ score) = test dices
                                                 newTable = addScore choice score $ ydTable ydata
                                             in return $ changeTable newTable ydata

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


iothingy :: YLabel -> YData -> IO YData
iothingy SelectDices = confirmSelection <=< askForSelection <=< displayState
iothingy ChooseWhereToScore = chooseWhereToScore <=< displayState
iothingy InitialThrow = return
iothingy AskWantToScore = askIfWantToScore <=< displayState
iothingy Rethrow = return
iothingy AskNumOfPlayers = askNumOfPlayers
iothingy SwitchPlayer = return
iothingy Final = displayCompleteTables

simpleIOMain :: IO ()
simpleIOMain = do ydata <- makeYData
                  _ <- executeTransition ydata trAskNumOfPlayers iothingy
                  return ()
                     



