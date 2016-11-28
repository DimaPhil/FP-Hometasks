{-# LANGUAGE OverloadedStrings #-}
module InteractiveCmd where

import           Control.Applicative       (pure, (<*>))
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT (..), get, modify)
import           Data.Map                  as M (Map (), fromList, insert,
                                                 lookup, toList)
import           Data.Maybe                (isJust)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text (), pack, splitOn, unpack,
                                            unwords)
import           Data.Text.IO              as T (putStrLn)
import           Prelude                   hiding (unwords)
import           System.Environment        (getArgs)
import           System.IO                 (IOMode (WriteMode), hPutStrLn,
                                            putStrLn, stdout, withFile)

splitProperty :: Text -> Text -> (Text, Text)
splitProperty ptrn text = (head parts, unwords $ tail parts)
  where
    parts = splitOn ptrn text

addToMap :: Text -> Text -> StateT (M.Map Text Text) IO ()
addToMap name value = modify (insert name value)

addItem :: (Text, Text) -> StateT (M.Map Text Text) IO ()
addItem item@(name, value) = do
    properties <- get
    if isJust (M.lookup name properties) then
        lift $ T.putStrLn $ "Key \"" <> name <> "\" already exists"
    else do
        addToMap name value
        lift $ T.putStrLn $ "(" <> name <> ", " <> value <> ") successfully added"

modifyItem :: (Text, Text) -> StateT (M.Map Text Text) IO ()
modifyItem (name, value) = do
    properties <- get
    if isJust (M.lookup name properties) then do
        addToMap name value
        lift $ T.putStrLn $ "Value for key " <> name <> " was modified to " <> value
    else
        lift $ T.putStrLn $ "There is no key " <> name <> ", can't modify value for it"

saveToFile :: Text -> StateT (M.Map Text Text) IO ()
saveToFile filename = do
    properties <- get
    let allData = map (\p@(name, value) -> unpack name ++ "=" ++ unpack value) (M.toList properties)
    let fn = unpack filename
    lift $ withFile fn WriteMode (\line -> mapM_ (hPutStrLn line) allData)
    lift $ T.putStrLn $ "All data was saved to \"" <> pack fn <> pack "\""

quit :: IO ()
quit = return ()

wrongCommand :: Text -> IO ()
wrongCommand command = T.putStrLn $ "Wrong command: " <> command

runCmd :: StateT (M.Map Text Text) IO ()
runCmd = do
    lift $ putStr "> "
    line <- lift getLine
    let (command, left) = splitProperty (pack " ") (pack line)
    let item@(name, value) = splitProperty (pack " ") left
    props <- get
    case unpack command of
        "+"  -> addItem (name, value) >> runCmd
        ":m" -> modifyItem (name, value) >> runCmd
        ":w" -> saveToFile left >> runCmd
        ":q" -> lift quit
        _    -> lift (wrongCommand (pack line)) >> runCmd

cmd :: IO ()
cmd = do
    args <- getArgs
    when (length args /= 1) $ T.putStrLn "Usage: cmd <filename>"
    when (length args == 1) $ do
        let filename = head args
        properties <- do
            fileLines <- pure lines <*> readFile filename
            let propertiesLines = map (splitProperty (pack "=") . pack) fileLines
            return (M.fromList propertiesLines)
        runStateT runCmd properties
        return ()
