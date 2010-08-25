{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Logic where

import Game.HYahtzee.Engine.Model

import Test.QuickCheck.Gen (Gen, choose, unGen)
import System.Random

rollDices :: Int -> Gen [DiceVal]
rollDices 0 = do return []
rollDices x = do dices <- rollDices (x-1)
                 dice <- choose (1,6)
                 return (dice:dices)

displayDice :: Gen DiceVal -> IO ()
displayDice dice = do gen <- getStdGen
                      let (gen1, gen2) = split gen
                          val = unGen dice gen1 1
                      putStr $ show val
                      setStdGen gen2

privDisplayDices :: Gen [DiceVal] -> IO ()
privDisplayDices gdices =
  do gen1 <- getStdGen
     let dices = unGen gdices gen1 1
     case dices of
       (d1:rest@(_:_)) -> do displayDice (return d1)
                             putStr ", "
                             privDisplayDices (return rest)
       (d1:[])         -> do displayDice (return d1)
       _               -> return ()
     
displayDices :: Gen [DiceVal] -> IO ()
displayDices dices = do putStr "{"
                        privDisplayDices dices 
                        putStrLn "}"

oneThrow :: YTable -> IO (Gen [DiceVal])
oneThrow _ = let dices = rollDices 5
             in do displayDices dices >> return dices

oneTurn :: YTable -> IO YTable
oneTurn table = do oneThrow table >> return table
  

onePlayer :: IO YTable -> IO YTable
onePlayer table = do mytable <- table
                     -- let full = isTableFull mytable
                     -- if full
                     --   then table
                     --   else 
                     -- onePlayer $ 
                     oneTurn mytable

mainOnePlayer :: IO YTable
mainOnePlayer = do onePlayer (return makeTable :: IO YTable)
