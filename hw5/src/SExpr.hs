module SExpr where

import           AParser             (Parser (), char, posInt, satisfy)
import           Control.Applicative ((<$>), (<*>), (<|>))
import           Data.Char           (isAlpha, isAlphaNum, isSpace)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = pure (:) <*> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = pure (:) <*> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = pure (:) <*> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String
data Atom = N Integer
          | I Ident
          deriving Show
data SExpr = A Atom
           | Comb [SExpr]
           deriving Show

parseSExpr :: Parser SExpr
parseSExpr = pure A <*> ((pure N <*> posInt) <|> (pure I <*> ident)) <|>
             pure Comb <*> (spaces *> char '(' *> spaces *>
                            oneOrMore (parseSExpr <* spaces) <* char ')' <* spaces)
