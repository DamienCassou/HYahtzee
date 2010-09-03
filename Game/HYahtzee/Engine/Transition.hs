{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Transition where

data Transition a = TransNorm String (a -> a) (Choice a)
                  | TransFinal String (a -> a)
                  
data Choice a = Choice (a -> Bool) (Transition a) (Transition a)

executeTransition :: a -> Transition a -> (String -> a -> IO a) -> IO a
executeTransition state (TransNorm name f choice) io = 
  do newState <- ((io name) . f) state
     executeChoice newState choice io
executeTransition state (TransFinal name f) io = 
  (io name . f) state
  
executeChoice :: a -> Choice a -> (String -> a -> IO a) -> IO a
executeChoice state (Choice test t1 t2) io =
  if test state
  then executeTransition state t1 io
  else executeTransition state t2 io

{- Example -}
mytran :: Transition Integer
mytran = TransNorm "mytran"
          (\x -> x - 1)
          (Choice (> 1) mytran (TransFinal "" id))

iothingy_ :: String -> Integer -> IO Integer
iothingy_ "mytran" i = print i >> return i
iothingy_ _ i = return i

main :: IO ()
main = executeTransition 6 mytran iothingy_ >> return ()