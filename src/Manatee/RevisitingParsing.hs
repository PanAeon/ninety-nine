{-# LANGUAGE InstanceSigs #-}

module Manatee.RevisitingParsing
    (
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.State.Strict
import Control.Monad (guard)
import Data.Char (isSpace, isDigit, ord)

-- FIXME the original paper.
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf

-- FIXME: relation between SateT and Parser?

-- from http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/

{-
A Parser for Things
is a function from Strings
to Lists of Pairs
of Things and Strings!

-}

{-
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Parser a = Parser (StateT String Maybe a) deriving (Functor, Applicative, Alternative, Monad)
-}
newtype Parser a = Parser { unParser :: StateT String Maybe a }
runParser = runStateT . unParser

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ f <$> unParser p

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ pure a
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> a = Parser $ unParser f <*> unParser a

instance Alternative Parser where
  empty :: Parser a
  empty = Parser empty
  (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = Parser $ unParser a <|> unParser b

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  a >>= f = Parser $ StateT $ \s -> do
     (a', s') <- runParser a s
     runParser (f a') s'

anyChar :: Parser Char
anyChar = Parser . StateT $ \s -> case s of
   [] -> empty
   (c:cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- anyChar
  guard $ pred c
  pure c

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) <|> pure a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
    where
        scan   = p >>= rest
        rest a = (do
            f <- op
            b <- scan
            rest (f a b)) <|> pure a

space :: Parser String
space = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = p <* space

symbol :: String -> Parser String
symbol = token . string

apply :: Parser a -> String -> Maybe (a, String)
apply p = runParser (space *> p)


expr, term, factor, digit :: Parser Int
expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
factor = digit <|> (symbol "(" *> expr <* symbol ")")
digit  = subtract (ord '0') . ord <$> token (satisfy isDigit)

addop, mulop :: Parser (Int -> Int -> Int)
addop = (symbol "+" *> pure (+)) <|> (symbol "-" *> pure (-))
mulop = (symbol "*" *> pure (*)) <|> (symbol "/" *> pure (div))

test = runParser expr "(1 + 2 * 4) / 3 + 5"
