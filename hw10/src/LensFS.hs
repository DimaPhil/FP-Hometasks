module LensFS where

import Control.Lens

data FS = Dir { name     :: FilePath
              , contents :: [FS]
              }
        | File { name    :: FilePath
               }
        deriving (Show)
