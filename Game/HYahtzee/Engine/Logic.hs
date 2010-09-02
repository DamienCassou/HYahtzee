{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination

import System.Random (getStdGen, randomRs)
  
data YData = YData { ydTable :: YTable
                   , ydRandoms :: [DiceVal]
                   , remainingThrows :: Int
                   }

max_throws :: Int
max_throws = 3

makeYData :: IO YData
makeYData = do gen <- getStdGen
               let randomValues = (randomRs (1,6) gen)
               return (YData makeTable randomValues max_throws)

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

changeTable :: YTable -> YData -> YData
changeTable table ydata = ydata {ydTable = table}

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




mainloop ydata =
  if isFull ydata
  then return ()
  else if canThrowDices ydata
       then do let newYData1 = throwDices ydata
                   displayDices newYData1
       else do let newYData1 = chooseCombination ydata
          
           
  
-- getNextMove s = do
--  print $ pretty s
--  l <- getLine
--  return $ parse l

askForChange :: YData -> IO String

mainOnePlayer :: IO ()
mainOnePlayer = do ydata <- makeYData
                   mainloop ydata
                     



