{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination

import System.Random (getStdGen, randomRs)
  
data YData = YData { ydMessages :: [String]
                   , ydInput :: [String]
                   , ydTable :: YTable
                   , ydRandoms :: [DiceVal]
                   }

makeYData :: IO YData
makeYData = do gen <- getStdGen
               inputStr <- getContents
               let inputLines = lines inputStr
                   randomValues = (randomRs (1,6) gen)
               return (YData [] inputLines makeTable randomValues)

addMessage :: String -> YData -> YData
addMessage message ydata = 
  ydata { ydMessages = (ydMessages ydata ++ [message]) }

addMessages :: [String] -> YData -> YData
addMessages [] ydata       = ydata
addMessages (f:rest) ydata = addMessages rest $ addMessage f ydata

printMessages :: YData -> IO ()
printMessages ydata = putStrLn $ unlines $ ydMessages ydata

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

readLine :: YData -> (String, YData)
readLine ydata  = let line = head $ ydInput ydata
                      rest = tail $ ydInput ydata
                      newData = ydata {ydInput = rest}
                      newData2 = addMessage line newData -- forces evaluation
                  in (line, newData2)

changeTable :: YTable -> YData -> YData
changeTable table ydata = ydata {ydTable = table}

displayDices :: [DiceVal] -> YData -> YData
displayDices dices = 
  addMessage (privDisp dices "{") where
    privDisp :: [DiceVal] -> String -> String
    privDisp [] s = s
    privDisp (l:[]) s = s ++ show l ++ "}"
    privDisp (d:rest)  s = privDisp rest (s ++ show d ++ ", ")

requestChoice :: [String] -> YData -> (String, YData)
requestChoice choices  ydata = 
  let prettyChoices = map -- prefix each choice by a number to be typed by the user
                      (\(f,s) -> show s ++ "- " ++ f)
                      (zip choices ([1..] :: [Integer]))
      newData1 = addMessages prettyChoices ydata
      newData2 = addMessage
                 ("Your choice between 1 and " ++ (show $ length choices)) newData1
      (input, newData3) = readLine newData2
      choice = choices !! ((read input) - 1)
      newData4 = addMessage ("You choosed " ++ choice) newData3
  in (choice, newData4)

list2dices :: [DiceVal] -> Dices
list2dices (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
list2dices _                           = (0,0,0,0,0)

chooseWhereToScore :: [DiceVal] -> YData -> YData
chooseWhereToScore dices ydata = 
  let freeCombs = freeCombinations $ ydTable ydata
      (choiceName, ydata2) = requestChoice freeCombs ydata
  in case combinationNamed choiceName of
    Just choice -> let (CombinationResult _ score) = (choice $ list2dices dices)
                       curTable = ydTable ydata2
                       newTable = addScore choiceName score curTable
                   in changeTable newTable ydata2
    Nothing      -> chooseWhereToScore dices ydata2

wantToContinue :: YData -> (Bool, YData)
wantToContinue ydata = let newYData1 = addMessage "Do you want to throw dices again?" ydata
                           (input, newYData2) = requestChoice ["Yes","No"] newYData1
                       in (input == "Yes", newYData2)
                          

onePlayerLogic :: YData -> YData
onePlayerLogic ydata =
  let (dices, newYData1) = consumeRandoms 5 ydata
      newYData2 = displayDices dices newYData1
      (throwAgain, newYData3) = wantToContinue newYData2
  in if throwAgain
     then onePlayerLogic newYData3
     else newYData3


-- next m init = m

mainloop :: a -> (a -> IO b) -> (b -> a -> a) -> (a -> Bool) -> IO ()
mainloop state askForChange changeState isDone = 
  do change <- askForChange state
     let newState = changeState change state
     if isDone newState
       then return ()
       else mainloop newState askForChange changeState isDone
  
-- getNextMove s = do
--  print $ pretty s
--  l <- getLine
--  return $ parse l

mainOnePlayer :: IO ()
mainOnePlayer = do ydata <- makeYData
                   printMessages $ onePlayerLogic ydata



