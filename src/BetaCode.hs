{-# LANGUAGE OverloadedStrings #-}

module BetaCode (fromBetaCode, toBetaCode) where

import qualified Data.ByteString as BS
import           Data.Word
import           BetaCode.Parser
import           BetaCode.Rules

fromBetaCode :: BS.ByteString -> Maybe BS.ByteString
fromBetaCode input = snd <$> runParser fromParser (BS.unpack input)

toBetaCode :: BS.ByteString -> Maybe BS.ByteString
toBetaCode input = snd <$> runParser toParser (BS.unpack input)

fromParser :: Parser BS.ByteString
fromParser = BS.pack <$> (setP parsers)
  where parsers = (uncurry buildParser) <$> fromRules

toParser :: Parser BS.ByteString
toParser = BS.pack <$> (setP parsers)
  where parsers = (uncurry buildParser) <$> toRules
