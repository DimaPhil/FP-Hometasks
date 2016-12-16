{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module PrettyShow where

import           Language.Haskell.TH        (Exp (..), Q (..), conT, listE,
                                             varE)
import           Language.Haskell.TH.Syntax (Con (..), Dec (..), Info (..),
                                             Name, nameBase, reify)
import           SelN                       (selN)

prettyShow :: Name -> Q [Dec]
prettyShow name = do
    TyConI (DataD _ _ _ _ [RecC ctr fields] _) <- reify name
    let names = map $(selN 3 1) fields
    let ctrName = show ctr
    let showField :: Name -> Q Exp
        showField name = [| \x -> s ++ " = " ++ show ($(varE name) x) |]
          where
            s = nameBase name
    let showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Show $(conT name) where
           show x = ctrName ++ " {\n" ++ intercalate ",\n" (map (("    " ++ ) . ($ x)) $showFields) ++ "\n}"|]

data A = A Int deriving (Show)
data MyData = MyData
  { foo :: A
  , bar :: Int
  }
