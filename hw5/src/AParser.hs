module AParser where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       (void, (>=>))
import           Data.Char           (isDigit, isUpper)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
    fmap f p = Parser $ \s -> first f <$> runParser p s

instance Applicative Parser where
    pure x    = Parser $ \s -> Just (x, s)
    p1 <*> p2 = Parser $ \s -> runParser p1 s >>= (\(x, y) -> runParser p2 y >>= (Just . first x))

instance Alternative Parser where
    empty     = Parser $ const Nothing
    p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
    f [] = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

charUppercase :: Parser Char
charUppercase = satisfy isUpper

posInt :: Parser Integer
posInt = Parser f where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

abParser :: Parser (Char, Char)
abParser = pure (,) <*> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure (\_ _ -> ()) <*> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = pure (\x _ y -> [x, y]) <*> posInt <*> char ' ' <*> posInt

intParser :: Parser ()
intParser = void posInt

charUppercaseParser :: Parser ()
charUppercaseParser = void charUppercase

intOrUppercase :: Parser ()
intOrUppercase = void (intParser <|> charUppercaseParser)
