{-# LANGUAGE OverloadedStrings #-}

module BetaCode (fromBetaCode, toBetaCode) where

import qualified Data.ByteString as BS
import           Data.Word
import           BetaCode.Parser
import           BetaCode.Rules

-- | Convert beta-code like string into proper unicode greek.
fromBetaCode :: BS.ByteString -> Maybe BS.ByteString
fromBetaCode input = snd <$> runParser fromParser (BS.unpack input)

-- | Convert proper unicode greek into beta-code like string.
toBetaCode :: BS.ByteString -> Maybe BS.ByteString
toBetaCode input = snd <$> runParser toParser (BS.unpack input)

fromParser :: Parser BS.ByteString
fromParser = BS.pack <$> (setP parsers)
  where parsers = (uncurry buildParser) <$> fromRules

toParser :: Parser BS.ByteString
toParser = BS.pack <$> (setP parsers)
  where parsers = (uncurry buildParser) <$> toRules
