module LensFS where

import           Control.Lens.Fold   (preview)
import           Control.Lens.Getter (view)
import           Control.Lens.Lens   (Lens' (..))
import           Control.Lens.Prism  (Prism' (..), prism')
import           Data.Functor        ((<$>))
import           Data.List           (isPrefixOf)
import           Data.Maybe          (catMaybes, fromMaybe)
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getDirectoryContents)

data FS = Dir  { name     :: FilePath
               , contents :: [FS]
               }
        | File { name :: FilePath
               }
        deriving (Show)

_file :: Prism' FS FilePath
_file = prism' File $ \fs ->
    case fs of
        File n -> Just n
        Dir _ _   -> Nothing

_dir :: Prism' FS (FilePath, [FS])
_dir = prism' (uncurry Dir) $ \fs ->
    case fs of
        Dir n c -> Just (n, c)
        File _  -> Nothing

_contents :: Lens' FS [FS]
_contents f fs = case fs of
                     Dir n c    -> (\_ -> Dir n c) <$> f c
                     t@(File _) -> const t <$> f []

scanDirectory :: FilePath -> IO (Maybe FS)
scanDirectory f = do
    isDirectory <- doesDirectoryExist f
    isFile      <- doesFileExist f
    if isFile then return $ Just (File f)
    else if isDirectory then do let contents =  getDirectoryContents f
                                let filContents = filter (not . isPrefixOf ".") <$> contents
                                recContents <- filContents >>= mapM scanDirectory
                                return $ Just (Dir f (catMaybes recContents))
         else return Nothing

subFS :: FS -> [FS]
subFS d = maybe [] snd (preview _dir d)

ls :: FS -> [FS]
ls = view _contents

file :: FS -> FilePath
file f = fromMaybe "" $ preview _file f
