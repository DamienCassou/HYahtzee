{-# OPTIONS -Wall #-}

module Game.HYahtzee.Engine.Transition where

data Transition l a = TransNorm l (a -> a) (Choice l a)
                    | TransFinal l (a -> a)
                  
data Choice l a = Choice (a -> Bool) (Transition l a) (Transition l a)

executeTransition :: (Monad m) => a -> Transition t a -> (t -> a -> m a) -> m a
executeTransition state (TransNorm name f choice) io = 
  do newState <- (io name . f) state
     executeChoice newState choice io
executeTransition state (TransFinal name f) io = 
  (io name . f) state
  
executeChoice :: (Monad m) => a -> Choice t a -> (t -> a -> m a) -> m a
executeChoice state (Choice test t1 t2) io =
  if test state
  then executeTransition state t1 io
  else executeTransition state t2 io

{- Example -}
{-
mytran :: Transition String Integer
mytran = TransNorm "mytran"
          (\x -> x - 1)
          (Choice (> 1) mytran (TransFinal "" id))

iothingy_ :: String -> Integer -> IO Integer
iothingy_ "mytran" i = print i >> return i
iothingy_ _ i = return i

main :: IO ()
main = executeTransition 6 mytran iothingy_ >> return ()
-}