module InteractiveCmd where

import           Control.Applicative (pure, (<*>))
import           Control.Monad       (when)
import           Data.IORef          (IORef (), newIORef, readIORef, writeIORef)
import           Data.Text           (Text (), pack, splitOn, unpack, unwords)
import           Prelude             hiding (unwords)
import           System.Environment  (getArgs)
import           System.IO           (IOMode (WriteMode), hPutStrLn, putStrLn,
                                      stdout, withFile)

splitProperty :: Text -> Text -> (Text, Text)
splitProperty ptrn text = (head parts, unwords $ tail parts)
  where
    parts = splitOn ptrn text

addItem :: IORef [(Text, Text)] -> (Text, Text) -> IO ()
addItem props item@(name, value) = do
    properties <- readIORef props
    if name `elem` map fst properties then
        putStrLn $ "Key \"" ++ show name ++ "\" already exists"
    else do
        let newProperties = item : properties
        writeIORef props newProperties
        putStrLn $ "(" ++ show name ++ ", " ++ show value ++ ") successfully added"
    runCmd props

modifyItem :: IORef [(Text, Text)] -> (Text, Text) -> IO ()
modifyItem props (name, value) = do
    properties <- readIORef props
    if name `elem` map fst properties then do
        let newProperties = map (\p@(n, _) -> if n == name then (n, value) else p) properties
        writeIORef props newProperties
        putStrLn $ "Value for key " ++ show name ++ " was modified to " ++ show value
    else
        putStrLn $ "There is no key " ++ show name ++ ", can't modify value for it"
    runCmd props

saveToFile :: IORef [(Text, Text)] -> Text -> IO ()
saveToFile props filename = do
    properties <- readIORef props
    let allData = map (\p@(name, value) -> unpack name ++ "=" ++ unpack value) properties
    let fn = unpack filename
    withFile fn WriteMode (\line -> mapM_ (hPutStrLn line) allData)
    putStrLn $ "All data was saved to \"" ++ fn ++ "\""
    runCmd props

quit :: IO ()
quit = return ()

wrongCommand :: IORef [(Text, Text)] -> Text -> IO ()
wrongCommand props command = do
    putStrLn $ "Wrong command: " ++ unpack command
    runCmd props

runCmd :: IORef [(Text, Text)] -> IO ()
runCmd props = do
    putStr "> "
    line <- getLine
    let (command, left) = splitProperty (pack " ") (pack line)
    let item@(name, value) = splitProperty (pack " ") left
    case unpack command of
        "+"  -> addItem props (name, value)
        ":m" -> modifyItem props (name, value)
        ":w" -> saveToFile props left
        ":q" -> quit
        _    -> wrongCommand props (pack line)

cmd :: IO ()
cmd = do
    args <- getArgs
    when (length args /= 1) $ putStrLn "Usage: cmd <filename>"
    when (length args == 1) $ do
        let filename = head args
        properties <- do
            fileLines <- pure lines <*> readFile filename
            let propertiesLines = map (splitProperty (pack "=") . pack) fileLines
            return propertiesLines
        props <- newIORef properties
        runCmd props
