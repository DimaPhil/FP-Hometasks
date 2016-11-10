module ArithmeticExpressions where

import           Control.Monad        (liftM2)
import           Control.Monad.Reader (Reader (), ask, local)
import           Data.Map             (Map (), insert, lookup)
import           Prelude              hiding (lookup)

data Expr = Const Int
          | Variable String
          | Sum Expr Expr
          | Multiply Expr Expr
          | Binding Expr String Int
          deriving Show

eval :: Expr -> Reader (Map String Int) (Maybe Int)
eval (Const x)              = return (Just x)
eval (Variable name)        = ask >>= \m -> return (lookup name m)
eval (Sum l r)              = eval l >>= \vl -> eval r >>= \vr -> return $ liftM2 (+) vl vr
eval (Multiply l r)         = eval l >>= \vl -> eval r >>= \vr -> return $ liftM2 (*) vl vr
eval (Binding e name value) = local (insert name value) (eval e)
