{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model
import Game.HYahtzee.Engine.Combination

data YData = YData { ydMessages :: [String]
                   , ydTable :: YTable
                   , ydRandoms :: [DiceVal]
                   }

addMessage :: String -> YData -> YData
addMessage message ydata = 
  ydata { ydMessages = (ydMessages ydata ++ [message]) }

consumeRandoms :: Int-> YData -> ([DiceVal], YData)
consumeRandoms num ydata = let (taken, rest) = splitAt num $ ydRandoms ydata
                               newData = ydata {ydRandoms = rest}
                           in (taken,newData)

changeTable :: YTable -> YData -> YData
changeTable table ydata = ydata {ydTable = table}

displayDices :: [DiceVal] -> YData -> YData
displayDices dices = 
  addMessage (privDisp dices "{") where
    privDisp :: [DiceVal] -> String -> String
    privDisp [] s = s
    privDisp (l:[]) s = s ++ show l ++ "}"
    privDisp (d:rest)  s = privDisp rest (s ++ show d ++ ", ")

-- TODO
displayChoices :: [String] -> YData -> (String, YData)
displayChoices choices ydata = (head choices, ydata)

list2dices :: [DiceVal] -> Dices
list2dices (d1:(d2:(d3:(d4:(d5:[]))))) = (d1,d2,d3,d4,d5)
list2dices _                           = (0,0,0,0,0)

chooseWhereToScore :: [DiceVal] -> YData -> YData
chooseWhereToScore dices ydata = 
  let freeCombs = freeCombinations $ ydTable ydata
      (choiceName, ydata2) = displayChoices freeCombs ydata
  in case combinationNamed choiceName of
    Just choice -> let (CombinationResult _ score) = (choice $ list2dices dices)
                       curTable = ydTable ydata2
                       newTable = addScore choiceName score curTable
                   in changeTable newTable ydata2
    Nothing      -> chooseWhereToScore dices ydata2

-- TODO
wantToContinue :: YData -> (Bool, YData)
wantToContinue ydata = (True, ydata)

-- TODO
onePlayerLogic2 :: [DiceVal] -> YData -> YData
onePlayerLogic2 _ ydata = ydata

onePlayerLogic :: YData -> YData
onePlayerLogic ydata
  | isTableFull (ydTable ydata) = ydata
  | otherwise                   = 
    let (dices, ydata2)  = consumeRandoms 5 ydata
        ydata3             = displayDices dices ydata2
        (continue, ydata4) = wantToContinue ydata3
    in if continue
       then onePlayerLogic2 dices ydata4
       else onePlayerLogic $ chooseWhereToScore dices ydata4
            

-- printMessages :: [String] -> IO ()
-- printMessages (m:rest) = do putStrLn m
--                             printMessages rest
-- printMessages []       = return ()                            

-- mainOnePlayer :: IO ()
-- mainOnePlayer = do gen <- getStdGen
--                    inputStr <- getContents
--                    printMessages $ onePlayerLogic makeTable (randomRs (1,6) gen) inputStr
