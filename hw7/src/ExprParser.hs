module ExprParser where

import           Control.Applicative   (pure, (*>), (<$>), (<*>))
import           Control.Monad         (replicateM_, void)
import           Control.Monad.Reader  (Reader (), asks, liftM2, runReader)
import           Data.Map              (Map (), empty, fromList, insert, lookup)
import           Data.Maybe            (fromJust, fromMaybe)
import           Data.Text             (Text (), pack, unpack)
import           Options.Applicative   (Parser (), command, execParser,
                                        fullDesc, header, help, helper, info,
                                        long, metavar, progDesc, strOption,
                                        subparser, (<>))
import           Prelude               hiding (lookup)
import           Text.Megaparsec       (alphaNumChar, between, letterChar, many,
                                        parseMaybe, runParser, sepBy1,
                                        spaceChar, try, (<|>))
import           Text.Megaparsec.Expr  (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Lexer as L (integer, lexeme, skipBlockComment,
                                             skipLineComment, space, symbol)
import qualified Text.Megaparsec.Text  as MP (Parser ())

data Expr = Variable Text
          | Const Integer
          | Neg Expr
          | Binary BinOperation Expr Expr
            deriving (Show)

data BinOperation = Add
                  | Subtract
                  | Multiply
                  | Divide
                  | Power
                    deriving (Show)

sc :: MP.Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: MP.Parser a -> MP.Parser a
lexeme = L.lexeme sc

symbol :: Text -> MP.Parser Text
symbol c = pure pack <*> L.symbol sc (unpack c)

parens :: MP.Parser a -> MP.Parser a
parens = between (symbol $ pack "(") (symbol $ pack ")")

integer :: MP.Parser Integer
integer = lexeme L.integer

identifier :: MP.Parser Text
identifier = pack <$> (lexeme . try) p where
    p = pure (:) <*> letterChar <*> many alphaNumChar

term :: MP.Parser Expr
term = parens exprParser
    <|> pure Variable <*> identifier
    <|> pure Const <*> integer

parsePair :: MP.Parser (Text, Integer)
parsePair = try p where
    p = pure (\x y -> (x, y)) <* symbol (pack "(") <*>
        identifier <* symbol (pack ",") <*> integer <* symbol (pack ")")

pairs :: MP.Parser [(Text, Integer)]
pairs = sepBy1 parsePair $ symbol (pack ",")

parsePairs :: Text -> Map Text Integer
parsePairs s = fromList result where
    result = case runParser pairs "" s of
        Left _ -> undefined
        Right lst -> lst

operators :: [[Operator MP.Parser Expr]]
operators =
  [ [ Prefix (symbol (pack "-") *> pure Neg) ]
  , [ InfixR (symbol (pack "^") *> pure (Binary Power)) ]
  , [ InfixL (symbol (pack "*") *> pure (Binary Multiply))
    , InfixL (symbol (pack "/") *> pure (Binary Divide)) ]
  , [ InfixL (symbol (pack "+") *> pure (Binary Add))
    , InfixL (symbol (pack "-") *> pure (Binary Subtract)) ]
  ]

exprParser :: MP.Parser Expr
exprParser = makeExprParser term operators

eval :: Expr -> Reader (Map Text Integer) (Maybe Double)
eval (Const x)             = return (Just $ fromIntegral x)
eval (Variable name)       = asks (fmap fromIntegral . lookup name)
eval (Neg e)               = fmap (fmap negate) (eval e)
eval (Binary Add l r)      = liftM2 (liftM2 (+)) (eval l) (eval r)
eval (Binary Subtract l r) = liftM2 (liftM2 (-)) (eval l) (eval r)
eval (Binary Multiply l r) = liftM2 (liftM2 (*)) (eval l) (eval r)
eval (Binary Divide l r)   = liftM2 (liftM2 (/)) (eval l) (eval r)
eval (Binary Power l r)    = liftM2 (liftM2 (**)) (eval l) (eval r)

data Output = Eval | PrintAst deriving (Show)
data ExpressionInfo = ExpressionInfo
    { output :: Output
    , expr   :: Text
    , values :: Text
    } deriving (Show)

parseExpression :: ExpressionInfo -> IO ()
parseExpression (ExpressionInfo output expr values) = case output of
    Eval     -> print $ fromJust (runReader (eval representation)
                                                     (parsePairs values))
    PrintAst -> print representation
    where
        representation = fromMaybe (error "Wrong expression") (parseMaybe exprParser expr)

einfo :: Parser ExpressionInfo
einfo = pure ExpressionInfo
  <*> subparser (
      command "eval" (info (helper <*> pure Eval)
                           (fullDesc <> progDesc "Evaluate expression")) <>
      command "print-ast" (info (helper <*> pure PrintAst)
                                (fullDesc <> progDesc "Print parsed expression")))
  <*> (pure pack <*> strOption (long "expr" <>
                                metavar "EXPRESSION" <>
                                help "Expression for parsing"))
  <*> (pure pack <*> strOption (long "var" <>
                                metavar "VALUES" <>
                                help "Values for the expression"))

parseIO :: IO ()
parseIO = parseExpression =<< execParser opts
  where
    opts = info (helper <*> einfo)
      (fullDesc
     <> progDesc "Enter expression and values for evaluating...")
