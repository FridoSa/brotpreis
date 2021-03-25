module Lib
    ( view, handleFileNotExists
    ) where

import System.IO.Error

handleFileNotExists :: IOError -> IO ()
handleFileNotExists e 
    | isDoesNotExistError e = putStrLn "No ingredients saved."
    | otherwise = ioError e

view :: IO ()
view = do  
    contents <- readFile "ingredients.txt"  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

