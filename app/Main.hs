module Main where

import Lib

dispatch :: [(Char, IO ())]
dispatch = [ ('c', calculate)
           , ('C', calculate)
           , ('e', edit)
           , ('E', edit)
           ]

main :: IO ()
main = do
    putStrLn "<C>alculate a price or <e>dit ingridients."
    char <- getChar
    getAction $ lookup char dispatch

getAction :: Maybe (IO ()) -> IO ()
getAction (Just action) = action
getAction Nothing = putStrLn "Illegal character given."

calculate :: IO ()
calculate = putStrLn "calculate"

edit :: IO ()
edit = putStrLn "edit"
