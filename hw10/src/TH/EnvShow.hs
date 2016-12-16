module EnvShow where

import           Language.Haskell.TH (Exp (..), Lit (..), Q (..), runIO)
import           System.Environment  (getEnv)

envTemplate :: Q Exp
envTemplate = do
    t <- runIO $ getEnv "TH_ENV"
    return $ LitE (StringL t)
