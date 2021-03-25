module Calculate
    (calculate
    ) where

import Lib
import System.IO
import System.IO.Error
import Text.Read

calculate :: IO ()
calculate = do
    view `catchIOError` handleFileNotExists
    putStrLn "Type a number followed by a quantity to add an ingredient. Commit with <return>."
    putStrLn "Press <Ctrl>+D to stop adding ingredients."
    tty <- openFile "/dev/tty" ReadMode
    contents <- hGetContents tty
    stored <- readFile "ingredients.txt"
    let ingredients = lines contents
        prices = map (calc stored . words) ingredients
        price = sumMaybe prices
    case price of Just money -> do
                      putStrLn ""
                      putStrLn ("The costs for the bread are: " ++ show money)
                  _ -> do
                      putStrLn ""
                      putStrLn "Bad input."
    hClose tty

calc :: String -> [String] -> Maybe Float
calc _ [] = Nothing
calc _ (_:[]) = Nothing
calc stored (x:y:_) = let maybeNumber = readMaybe x
                          maybeQuantity = readMaybe y
                      in
                      case (maybeNumber, maybeQuantity) of (Just number, Just quantity) -> case getUnitPrice stored number of Just price -> Just (price*quantity)
                                                                                                                              _ -> Nothing
                                                           _ -> Nothing

getUnitPrice :: String -> Int -> Maybe Float
getUnitPrice stored n = let items = lines stored
                        in
                        if n < length items
                            then let item = words (items !! n) in
                                 (Just ((read (item !! 2))/(read (item !! 1))))
                            else Nothing

sumMaybe :: (Num a) => [Maybe a] -> Maybe a
sumMaybe = foldl (\acc x -> case (acc, x) of (Just ac, Just num) -> Just (ac + num)
                                             _ -> Nothing) (Just 0)