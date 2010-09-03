{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Transition where

data Transition a = TransNorm (a -> a) (a -> IO a) (Choice a)
                  | TransFinal (a -> a) (a -> IO a)
                  
data Choice a = Choice (a -> Bool) (Transition a) (Transition a)

executeTransition :: a -> Transition a -> IO a
executeTransition state (TransNorm f io choice) =
  do newState <- (io . f) state
     executeChoice newState choice
executeTransition state (TransFinal f io) =
  (io . f) state
  
executeChoice :: a -> Choice a -> IO a
executeChoice state (Choice test t1 t2) =
  if test state
  then executeTransition state t1
  else executeTransition state t2
       

{- Example -}
mytran :: Transition Integer
mytran = TransNorm
          (\x -> x - 1)
          (\x -> print x >> return x)
          (Choice (> 1)
           mytran
           (TransFinal id return))
         
main :: IO ()
main = executeTransition 6 mytran >> return ()