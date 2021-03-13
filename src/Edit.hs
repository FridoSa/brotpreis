module Edit
    (edit
    ) where

import Lib
import System.IO
import System.IO.Error

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
            ,("rm", remove)
            ,("ed", editEntry)
           ]

handelFileNotExists :: IOError -> IO ()
handelFileNotExists e 
    | isDoesNotExistError e = putStrLn "No ingredients saved."
    | otherwise = ioError e

getAction :: Maybe ([String] -> IO ()) -> [String] -> IO ()
getAction (Just action) arguments = action arguments
getAction Nothing _ = putStrLn "Illegal command."

edit :: IO ()
edit = do
    showIngredients `catchIOError` handelFileNotExists
    putStrLn "Type add followed by a new entry to add an item. Attributes must be whitespace-seperated."
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
add i = appendFile "ingredients.txt" (unwords i ++ "\n")

remove :: [String] -> IO ()
remove a = return ()

editEntry :: [String] -> IO ()
editEntry a = return ()
