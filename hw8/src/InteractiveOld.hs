{-# LANGUAGE OverloadedStrings #-}
module InteractiveCmd where

import           Control.Applicative       (pure, (<*>))
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT (..))
import           Data.IORef                (IORef (), modifyIORef, newIORef,
                                            readIORef, writeIORef)
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

addToMap :: IORef (M.Map Text Text) -> Text -> Text -> IO ()
addToMap properties name value = modifyIORef properties (insert name value)

addItem :: IORef (M.Map Text Text) -> (Text, Text) -> IO ()
addItem props item@(name, value) = do
    properties <- readIORef props
    if isJust (M.lookup name properties) then
        T.putStrLn $ "Key \"" <> name <> "\" already exists"
    else do
        addToMap props name value
        T.putStrLn $ "(" <> name <> ", " <> value <> ") successfully added"

modifyItem :: IORef (M.Map Text Text) -> (Text, Text) -> IO ()
modifyItem props (name, value) = do
    properties <- readIORef props
    if isJust (M.lookup name properties) then do
        addToMap props name value
        T.putStrLn $ "Value for key " <> name <> " was modified to " <> value
    else
        T.putStrLn $ "There is no key " <> name <> ", can't modify value for it"

saveToFile :: IORef (M.Map Text Text) -> Text -> IO ()
saveToFile props filename = do
    properties <- readIORef props
    let allData = map (\p@(name, value) -> unpack name ++ "=" ++ unpack value) (M.toList properties)
    let fn = unpack filename
    withFile fn WriteMode (\line -> mapM_ (hPutStrLn line) allData)
    T.putStrLn $ "All data was saved to \"" <> pack fn <> pack "\""

quit :: IO ()
quit = return ()

wrongCommand :: IORef (M.Map Text Text) -> Text -> IO ()
wrongCommand props command = T.putStrLn $ "Wrong command: " <> command

runCmd :: IORef (Map Text Text) -> StateT (M.Map Text Text, M.Map Text Text) IO ()
runCmd props = do
    lift $ putStr "> "
    line <- lift getLine
    let (command, left) = splitProperty (pack " ") (pack line)
    let item@(name, value) = splitProperty (pack " ") left
    case unpack command of
        "+"  -> lift (addItem props (name, value)) >> runCmd props
        ":m" -> lift (modifyItem props (name, value)) >> runCmd props
        ":w" -> lift (saveToFile props left) >> runCmd props
        ":q" -> lift quit
        _    -> lift (wrongCommand props (pack line)) >> runCmd props

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
        props <- newIORef properties
        runStateT (runCmd props) (properties, properties)
        return ()
