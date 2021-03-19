module Edit
    (edit
    ) where

import Lib
import System.IO
import System.IO.Error
import System.Directory
import Data.List
import Text.Read

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
            ,("rm", remove)
            ,("ed", editEntry)
            ,("ret", (\_ -> return ()))
           ]

handleFileNotExists :: IOError -> IO ()
handleFileNotExists e 
    | isDoesNotExistError e = putStrLn "No ingredients saved."
    | otherwise = ioError e

getAction :: Maybe ([String] -> IO ()) -> [String] -> IO ()
getAction (Just action) arguments = action arguments
getAction Nothing _ = putStrLn "Illegal command."

edit :: IO ()
edit = do
    showIngredients `catchIOError` handleFileNotExists
    putStrLn "Type add followed by a new entry to add an item. Attributes must be whitespace-seperated."
    putStrLn "Type ret to return."
    command <- getLine
    case words command of (action : arguments) -> getAction (lookup action dispatch) arguments
                          [] -> putStrLn "No command given."
    edit
    
showIngredients :: IO ()
showIngredients = do
    view
    putStrLn "Type rm followed by a number to remove an item."
    putStrLn "Type ed followed by a number and a changed entry to edit an item. Attributes must be whitespace-seperated."

add :: [String] -> IO ()
add [] = do
    putStrLn "No item given."
add i = do
    appendFile "ingredients.txt" (unwords i ++ "\n")

remove :: [String] -> IO ()
remove [] = do
    putStrLn "No number given."
remove (x:_) = do
    let maybeNumber = readMaybe x
        fileName = "ingredients.txt"
    case maybeNumber of (Just number) -> do
                            handle <- openFile fileName ReadMode  
                            (tempName, tempHandle) <- openTempFile "." "temp"  
                            contents <- hGetContents handle 
                            let items = lines contents  
                                newItems = if number < length items
                                            then delete (items !! number) items
                                            else items
                            hPutStr tempHandle $ unlines newItems  
                            hClose handle  
                            hClose tempHandle  
                            removeFile fileName  
                            renameFile tempName fileName
                        Nothing -> do
                            putStrLn "Bad input. Number needed."

editEntry :: [String] -> IO ()
editEntry [] = do
    putStrLn "No arguments given."
editEntry (x:[]) = do
    putStrLn "To few arguments given."
editEntry (n:i) = do
    remove [n]
    add i