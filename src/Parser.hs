module Parser (Parser, (<$>), (<|>), charP, stringP, runParser) where

import Control.Applicative
import Data.Word

newtype Parser a = Parser { runParser :: [Word8] -> Maybe ([Word8], a) }

instance Functor Parser where
  fmap f (Parser x) =
     Parser $ \input -> do
       (rest, value) <- x input
       return (rest, f value)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input',  a) <- p1 input
      (input'', b) <- p2 input'
      return (input'', a b)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Semigroup a => Semigroup (Parser a) where
  (Parser p1) <> (Parser p2) =
    Parser $ \input -> do
      (input',  a) <- p1 input
      (input'', b) <- p2 input'
      return (input'', a <> b)

charP :: Word8 -> Parser Word8
charP char = Parser f
  where f (c:cs) | c == char = Just (cs, c)
        f _                  = Nothing

stringP :: [Word8] -> Parser [Word8]
stringP text = sequenceA $ fmap charP text
