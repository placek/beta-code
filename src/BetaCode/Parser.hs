-- | Parser for further conversion.
module BetaCode.Parser (Parser, (<$>), setP, buildParser, runParser) where

import Control.Applicative
import Data.Word

-- | A @Parser@ type.
-- Wraps up the function `runParser` and treats it as a value.
newtype Parser a =
  Parser {
    -- | It performs parsing mechanism. It takes a list of bytes (`Word8`) and parses byte by byte, returning a tuple consisting of the unparsed bytes and a match.
    runParser :: [Word8] -> Maybe ([Word8], a)
         }

-- | A `Functor` instance for `Parser` type.
instance Functor Parser where
  fmap f (Parser x) =
     Parser $ \input -> do
       (rest, value) <- x input
       return (rest, f value)

-- | An `Applicative` instance for `Parser` type.
instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input',  a) <- p1 input
      (input'', b) <- p2 input'
      return (input'', a b)

-- | An `Alternative` instance for `Parser` type.
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

-- | Parse one byte. The matcher is a specific byte @char@ given as an argument.
charP :: Word8 -> Parser Word8
charP char = Parser f
  where f (c:cs) | c == char = Just (cs, c)
        f _                  = Nothing

-- | Parse a string of bytes. The matcher is a sequence @text@ of bytes given in argument.
stringP :: [Word8] -> Parser [Word8]
stringP text = sequenceA $ fmap charP text

-- | Parse a set. The matcher is one of the provided list of @parsers@. The oder of parsers in the list matters.
setP :: [Parser [Word8]] -> Parser [Word8]
setP parsers = concat <$> (many $ foldl (<|>) (head parsers) (tail parsers))

-- | Build parser that maches @pattern@ and when matched, presents it as @token@.
buildParser :: [Word8] -> [Word8] -> Parser [Word8]
buildParser pattern token = convert <$> parser
  where parser    = stringP pattern
        convert _ = token
