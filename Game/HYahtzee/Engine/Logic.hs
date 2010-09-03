{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination
import Game.HYahtzee.Engine.Transition

import Control.Monad
import System.Random (getStdGen, randomRs)
  
data YData = YData { ydTable :: YTable
                   , ydRandoms :: [DiceVal]
                   , ydDices :: [DiceVal]
                   , remainingThrows :: Int
                   , wantToScore :: Bool
                   }

maxThrows :: Int
maxThrows = 3

makeYData :: IO YData
makeYData = do gen <- getStdGen
               let randomValues = (randomRs (1,6) gen)
               return (YData makeTable randomValues [] maxThrows True)

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

throwDices :: YData -> YData
throwDices ydata = let (dices, newYData1) = consumeRandoms 5 ydata
                   in newYData1 {ydDices = dices}

changeTable :: YTable -> YData -> YData
changeTable table ydata = ydata {ydTable = table}

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
                      in table `forM_` (\(name,score) -> putStrLn $ name ++ "\t\t" ++ show score)

-- requestChoice :: [String] -> YData -> (String, YData)
-- requestChoice choices  ydata = 
--   let prettyChoices = map -- prefix each choice by a number to be typed by the user
--                       (\(f,s) -> show s ++ "- " ++ f)
--                       (zip choices ([1..] :: [Integer]))
--       newData1 = addMessages prettyChoices ydata
--       newData2 = addMessage
--                  ("Your choice between 1 and " ++ (show $ length choices)) newData1
--       (input, newData3) = readLine newData2
--       choice = choices !! ((read input) - 1)
--       newData4 = addMessage ("You choosed " ++ choice) newData3
--   in (choice, newData4)

list2dices :: [DiceVal] -> Dices
list2dices (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
list2dices _                           = (0,0,0,0,0)

isFull :: YData -> Bool
isFull = isTableFull . ydTable

request :: String -> IO Bool
request title = do putStrLn $ title ++ "[y/n]"
                   char <- getChar
                   case char of
                     'y' -> return True
                     'n' -> return False
                     _   -> request title

resetRemainingThrows :: YData -> YData
resetRemainingThrows ydata = ydata {remainingThrows = maxThrows}

chooseWhereToScore :: YData -> IO YData
chooseWhereToScore ydata = do putStrLn "Where do you want to score?"
                              line <- getLine
                              putStrLn $  "You choosed " ++ line
                              return ydata

decrementRemainingThrows :: YData -> YData
decrementRemainingThrows ydata = ydata {remainingThrows = remainingThrows ydata - 1}

askIfWantToScore :: YData -> IO YData
askIfWantToScore ydata = do want <- request "Do you want to score now?"
                            return ydata {wantToScore = want}
                            
chTableFull :: Choice YData
chTableFull = Choice
              isFull
              (TransFinal id return) 
              (TransNorm
               (throwDices . resetRemainingThrows)
               return
               chRemainThrows)

chRemainThrows :: Choice YData
chRemainThrows = Choice
                 ((> 0) . remainingThrows)
                 (TransNorm id askIfWantToScore chWantToScore)
                 (TransNorm id chooseWhereToScore chTableFull)

chWantToScore :: Choice YData
chWantToScore = Choice
                wantToScore
                (TransNorm id chooseWhereToScore chTableFull)
                (TransNorm
                 (throwDices . decrementRemainingThrows) 
                 return
                 chRemainThrows)

mainOnePlayer :: IO ()
mainOnePlayer = do ydata <- makeYData
                   _ <- executeChoice ydata chTableFull
                   return ()
                     



