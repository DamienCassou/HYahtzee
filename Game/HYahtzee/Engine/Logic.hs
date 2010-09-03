{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Transition

import Data.List (sort)
import Control.Monad
import System.Random (getStdGen, randomRs)
  
data YData = YData { ydTable :: YTable
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
               return (YData makeTable randomValues [] [] maxThrows True True)

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

throwDices :: YData -> YData
throwDices ydata = let (dices, newYData1) = consumeRandoms (5 - (length $ keptDices ydata)) ydata
                   in newYData1 {ydDices = dices ++ (keptDices ydata)}

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

displayState :: YData -> IO YData
displayState ydata = do displayTable $ ydTable ydata
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
        return $ choices !! (read input - 1)

list2dices :: [DiceVal] -> Dices
list2dices dices = list2dices_ $ sort dices where
  list2dices_ (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
  list2dices_ _                           = (0,0,0,0,0)

isFull :: YData -> Bool
isFull = isTableFull . ydTable

request :: String -> IO Bool
request title = do putStr $ title ++ " [y/n] "
                   line <- getLine
                   case head line of
                     'y' -> return True
                     'n' -> return False
                     _   -> request title

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
                                             in return ydata {ydTable = newTable}
between :: Int -> Int -> Int -> Bool
between min_ max_ val = (min_ <= val) && (val <= max_)

decrementRemainingThrows :: YData -> YData
decrementRemainingThrows ydata = ydata {remainingThrows = remainingThrows ydata - 1}

askIfWantToScore :: YData -> IO YData
askIfWantToScore ydata = do want <- request "Do you want to score now?"
                            return ydata {wantToScore = want}
                            
askForSelection :: YData -> IO YData
askForSelection ydata = do displayDices [1..5]
                           putStrLn "Type indices of dices to keep:"
                           line <- getLine
                           let indices = map (read . (: [])) line -- "123" -> [1,2,3]
                           if (all (between 1 6) indices)
                             then let dices = ydDices ydata
                                      kept = [ dices !! (index - 1) | index <- indices]
                                  in return ydata {keptDices = sort kept}
                             else askForSelection ydata

confirmSelection :: YData -> IO YData
confirmSelection ydata = do displayDices $ keptDices ydata
                            keep <- request $  "Do you want to keep these dices?"
                            return ydata {selectionIsOk = keep}

displayCompleteTable :: YData -> IO YData
displayCompleteTable ydata = do let table = calculateTotalAndBonus $ ydTable ydata::[(String,Score)]
                                table `forM_` (\(name, score) -> putStrLn $ name ++ "\t\t" ++ (show score))
                                return ydata

trSelectDices :: Transition YData
trSelectDices = (TransNorm id (confirmSelection <=< askForSelection <=< displayState) chSelection)

trChooseWhereToScore :: Transition YData
trChooseWhereToScore = (TransNorm id (chooseWhereToScore <=< displayState) chTableFull)

chTableFull :: Choice YData
chTableFull = Choice
              isFull
              (TransFinal id displayCompleteTable) 
              (TransNorm (throwDices . resetRemainingThrows . resetKeptDices) return chRemainThrows)

chRemainThrows :: Choice YData
chRemainThrows = Choice
                 ((> 1) . remainingThrows)
                 (TransNorm id (askIfWantToScore <=< displayState) chWantToScore)
                 trChooseWhereToScore

chWantToScore :: Choice YData
chWantToScore = Choice
                wantToScore
                trChooseWhereToScore
                trSelectDices

chSelection :: Choice YData
chSelection = Choice
              selectionIsOk
                (TransNorm (throwDices . decrementRemainingThrows) return chRemainThrows)
                trSelectDices

mainOnePlayer :: IO ()
mainOnePlayer = do ydata <- makeYData
                   _ <- executeChoice ydata chTableFull
                   return ()
                     



