{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseSExpr :: Parser SExpr
parseSExpr = spaces *> (atom <|> comb) <* spaces
  where
    atom = A <$> parseAtom
    comb = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')'

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)


exercise3 :: IO()
exercise3 = do
  print $ runParser (spaces *> posInt) "     345"
  print $ runParser parseSExpr "5"
  print $ runParser parseSExpr "foo3"
  print $ runParser parseSExpr "(bar (foo) 3 5 874)"
  print $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
  print $ runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"


main :: IO()
main = do
  exercise3
