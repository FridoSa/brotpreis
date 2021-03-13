module Lib
    ( view
    ) where


view :: IO ()
view = do  
    contents <- readFile "ingredients.txt"  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

