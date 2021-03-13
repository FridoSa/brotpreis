module Main where

import Edit
import Control.Monad

dispatch :: [(String, IO ())]
dispatch = [ ("c", calculate)
           , ("C", calculate)
           , ("e", edit)
           , ("E", edit)
           ]

main :: IO ()
main = forever $ do
    putStrLn "<C>alculate a price or <e>dit ingredients."
    char <- getLine
    getAction $ lookup char dispatch

getAction :: Maybe (IO ()) -> IO ()
getAction (Just action) = action
getAction Nothing = putStrLn "Illegal command."

calculate :: IO ()
calculate = putStrLn "calculate"

