import Game.HYahtzee.Engine.Logic

main :: IO ()
main = do ydata <- makeYData
          let (choice1, newData) = requestChoice ["a","b","c"] ydata
          let (choice2, newData2) = requestChoice ["a","b","c"] newData
          putStrLn $ unlines $ ydMessages newData2
