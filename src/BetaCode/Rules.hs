{-# LANGUAGE OverloadedStrings #-}

module BetaCode.Rules (fromRules, toRules) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as BS
import           Data.Bifunctor     (bimap)
import           Data.Word          (Word8)
import           Data.List          (sortBy)
import           Data.Tuple         (swap)

toRules :: [([Word8], [Word8])]
toRules = fmap swap rules
-- toRules = reverse $ fmap swap fromRules

fromRules :: [([Word8], [Word8])]
fromRules = reverse rules
-- fromRules = sortBy criteria rules
--   where criteria (a, _) (b, _) = compare b a

rules :: [([Word8], [Word8])]
rules = fmap (bimap encode encode) values
  where values = [ (" ",      " ")
                 , ("#",      "ʹ")
                 , ("'",      "᾽")
                 , ("(",      "ʽ")
                 , (")",      "ʼ")
                 , (",",      ",")
                 , ("*(/a",   "Ἅ")
                 , ("*(/e",   "Ἕ")
                 , ("*(/h",   "Ἥ")
                 , ("*(/i",   "Ἵ")
                 , ("*(/o",   "Ὅ")
                 , ("*(/u",   "Ὕ")
                 , ("*(/w",   "Ὥ")
                 , ("*(/|a",  "ᾍ")
                 , ("*(/|h",  "ᾝ")
                 , ("*(/|w",  "ᾭ")
                 , ("*(=a",   "Ἇ")
                 , ("*(=h",   "Ἧ")
                 , ("*(=i",   "Ἷ")
                 , ("*(=u",   "Ὗ")
                 , ("*(=w",   "Ὧ")
                 , ("*(=|a",  "ᾏ")
                 , ("*(=|h",  "ᾟ")
                 , ("*(=|w",  "ᾯ")
                 , ("*(\\a",  "Ἃ")
                 , ("*(\\e",  "Ἓ")
                 , ("*(\\h",  "Ἣ")
                 , ("*(\\i",  "Ἳ")
                 , ("*(\\o",  "Ὃ")
                 , ("*(\\u",  "Ὓ")
                 , ("*(\\w",  "Ὣ")
                 , ("*(\\|a", "ᾋ")
                 , ("*(\\|h", "ᾛ")
                 , ("*(\\|w", "ᾫ")
                 , ("*(a",    "Ἁ")
                 , ("*(e",    "Ἑ")
                 , ("*(h",    "Ἡ")
                 , ("*(i",    "Ἱ")
                 , ("*(o",    "Ὁ")
                 , ("*(r",    "Ῥ")
                 , ("*(u",    "Ὑ")
                 , ("*(w",    "Ὡ")
                 , ("*(|a",   "ᾉ")
                 , ("*(|h",   "ᾙ")
                 , ("*(|w",   "ᾩ")
                 , ("*)/a",   "Ἄ")
                 , ("*)/e",   "Ἔ")
                 , ("*)/h",   "Ἤ")
                 , ("*)/i",   "Ἴ")
                 , ("*)/o",   "Ὄ")
                 , ("*)/w",   "Ὤ")
                 , ("*)/|a",  "ᾌ")
                 , ("*)/|h",  "ᾜ")
                 , ("*)/|w",  "ᾬ")
                 , ("*)=a",   "Ἆ")
                 , ("*)=h",   "Ἦ")
                 , ("*)=i",   "Ἶ")
                 , ("*)=w",   "Ὦ")
                 , ("*)=|a",  "ᾎ")
                 , ("*)=|h",  "ᾞ")
                 , ("*)=|w",  "ᾮ")
                 , ("*)\\a",  "Ἂ")
                 , ("*)\\e",  "Ἒ")
                 , ("*)\\h",  "Ἢ")
                 , ("*)\\i",  "Ἲ")
                 , ("*)\\o",  "Ὂ")
                 , ("*)\\w",  "Ὢ")
                 , ("*)\\|a", "ᾊ")
                 , ("*)\\|h", "ᾚ")
                 , ("*)\\|w", "ᾪ")
                 , ("*)a",    "Ἀ")
                 , ("*)e",    "Ἐ")
                 , ("*)h",    "Ἠ")
                 , ("*)i",    "Ἰ")
                 , ("*)o",    "Ὀ")
                 , ("*)w",    "Ὠ")
                 , ("*)|a",   "ᾈ")
                 , ("*)|h",   "ᾘ")
                 , ("*)|w",   "ᾨ")
                 , ("*+i",    "Ϊ")
                 , ("*+u",    "Ϋ")
                 , ("*/a",    "Ά")
                 , ("*/e",    "Έ")
                 , ("*/h",    "Ή")
                 , ("*/i",    "Ί")
                 , ("*/o",    "Ό")
                 , ("*/u",    "Ύ")
                 , ("*/w",    "Ώ")
                 , ("*\\a",   "Ὰ")
                 , ("*\\e",   "Ὲ")
                 , ("*\\h",   "Ὴ")
                 , ("*\\i",   "Ὶ")
                 , ("*\\o",   "Ὸ")
                 , ("*\\u",   "Ὺ")
                 , ("*\\w",   "Ὼ")
                 , ("*a",     "Α")
                 , ("*a(",    "Ἁ")
                 , ("*a(/",   "Ἅ")
                 , ("*a(/|",  "ᾍ")
                 , ("*a(=",   "Ἇ")
                 , ("*a(=|",  "ᾏ")
                 , ("*a(\\",  "Ἃ")
                 , ("*a(\\|", "ᾋ")
                 , ("*a(|",   "ᾉ")
                 , ("*a)",    "Ἀ")
                 , ("*a)/",   "Ἄ")
                 , ("*a)/|",  "ᾌ")
                 , ("*a)=",   "Ἆ")
                 , ("*a)=|",  "ᾎ")
                 , ("*a)\\",  "Ἂ")
                 , ("*a)\\|", "ᾊ")
                 , ("*a)|",   "ᾈ")
                 , ("*a/",    "Ά")
                 , ("*a\\",   "Ὰ")
                 , ("*a|",    "ᾼ")
                 , ("*b",     "Β")
                 , ("*c",     "Ξ")
                 , ("*d",     "Δ")
                 , ("*e",     "Ε")
                 , ("*e(",    "Ἑ")
                 , ("*e(/",   "Ἕ")
                 , ("*e(\\",  "Ἓ")
                 , ("*e)",    "Ἐ")
                 , ("*e)/",   "Ἔ")
                 , ("*e)\\",  "Ἒ")
                 , ("*e/",    "Έ")
                 , ("*e\\",   "Ὲ")
                 , ("*f",     "Φ")
                 , ("*g",     "Γ")
                 , ("*h",     "Η")
                 , ("*h(",    "Ἡ")
                 , ("*h(/",   "Ἥ")
                 , ("*h(/|",  "ᾝ")
                 , ("*h(=",   "Ἧ")
                 , ("*h(=|",  "ᾟ")
                 , ("*h(\\",  "Ἣ")
                 , ("*h(\\|", "ᾛ")
                 , ("*h(|",   "ᾙ")
                 , ("*h)",    "Ἠ")
                 , ("*h)/",   "Ἤ")
                 , ("*h)/|",  "ᾜ")
                 , ("*h)=",   "Ἦ")
                 , ("*h)=|",  "ᾞ")
                 , ("*h)\\",  "Ἢ")
                 , ("*h)\\|", "ᾚ")
                 , ("*h)|",   "ᾘ")
                 , ("*h/",    "Ή")
                 , ("*h\\",   "Ὴ")
                 , ("*h|",    "ῌ")
                 , ("*i",     "Ι")
                 , ("*i(",    "Ἱ")
                 , ("*i(/",   "Ἵ")
                 , ("*i(=",   "Ἷ")
                 , ("*i(\\",  "Ἳ")
                 , ("*i)",    "Ἰ")
                 , ("*i)/",   "Ἴ")
                 , ("*i)=",   "Ἶ")
                 , ("*i)\\",  "Ἲ")
                 , ("*i+",    "Ϊ")
                 , ("*i/",    "Ί")
                 , ("*i\\",   "Ὶ")
                 , ("*j",     "Σ")
                 , ("*k",     "Κ")
                 , ("*l",     "Λ")
                 , ("*m",     "Μ")
                 , ("*n",     "Ν")
                 , ("*o",     "Ο")
                 , ("*o(",    "Ὁ")
                 , ("*o(/",   "Ὅ")
                 , ("*o(\\",  "Ὃ")
                 , ("*o)",    "Ὀ")
                 , ("*o)/",   "Ὄ")
                 , ("*o)\\",  "Ὂ")
                 , ("*o/",    "Ό")
                 , ("*o\\",   "Ὸ")
                 , ("*p",     "Π")
                 , ("*q",     "Θ")
                 , ("*r",     "Ρ")
                 , ("*r(",    "Ῥ")
                 , ("*s",     "Σ")
                 , ("*s1",    "Σ")
                 , ("*s2",    "Σ")
                 , ("*s3",    "Ϲ")
                 , ("*t",     "Τ")
                 , ("*u",     "Υ")
                 , ("*u(",    "Ὑ")
                 , ("*u(/",   "Ὕ")
                 , ("*u(=",   "Ὗ")
                 , ("*u(\\",  "Ὓ")
                 , ("*u+",    "Ϋ")
                 , ("*u/",    "Ύ")
                 , ("*u\\",   "Ὺ")
                 , ("*w",     "Ω")
                 , ("*w(",    "Ὡ")
                 , ("*w(/",   "Ὥ")
                 , ("*w(/|",  "ᾭ")
                 , ("*w(=",   "Ὧ")
                 , ("*w(=|",  "ᾯ")
                 , ("*w(\\",  "Ὣ")
                 , ("*w(\\|", "ᾫ")
                 , ("*w(|",   "ᾩ")
                 , ("*w)",    "Ὠ")
                 , ("*w)/",   "Ὤ")
                 , ("*w)/|",  "ᾬ")
                 , ("*w)=",   "Ὦ")
                 , ("*w)=|",  "ᾮ")
                 , ("*w)\\",  "Ὢ")
                 , ("*w)\\|", "ᾪ")
                 , ("*w)|",   "ᾨ")
                 , ("*w/",    "Ώ")
                 , ("*w\\",   "Ὼ")
                 , ("*w|",    "ῼ")
                 , ("*x",     "Χ")
                 , ("*y",     "Ψ")
                 , ("*z",     "Ζ")
                 , ("*|a",    "ᾼ")
                 , ("*|h",    "ῌ")
                 , ("*|w",    "ῼ")
                 , ("+",      " ̈")
                 , (",",      ",")
                 , ("-",      "-")
                 , (".",      ".")
                 , ("/",      " ́")
                 , (":",      "·")
                 , (";",      ";")
                 , ("=",      " ͂")
                 , ("\\",     "`")
                 , ("_",      "—")
                 , ("a",      "α")
                 , ("a(",     "ἁ")
                 , ("a(/",    "ἅ")
                 , ("a(/|",   "ᾅ")
                 , ("a(=",    "ἇ")
                 , ("a(=|",   "ᾇ")
                 , ("a(\\",   "ἃ")
                 , ("a(\\|",  "ᾃ")
                 , ("a(|",    "ᾁ")
                 , ("a)",     "ἀ")
                 , ("a)/",    "ἄ")
                 , ("a)/|",   "ᾄ")
                 , ("a)=",    "ἆ")
                 , ("a)=|",   "ᾆ")
                 , ("a)\\",   "ἂ")
                 , ("a)\\|",  "ᾂ")
                 , ("a)|",    "ᾀ")
                 , ("a/",     "ά")
                 , ("a/|",    "ᾴ")
                 , ("a=",     "ᾶ")
                 , ("a=|",    "ᾷ")
                 , ("a\\",    "ὰ")
                 , ("a\\|",   "ᾲ")
                 , ("a|",     "ᾳ")
                 , ("b",      "β")
                 , ("c",      "ξ")
                 , ("d",      "δ")
                 , ("e",      "ε")
                 , ("e(",     "ἑ")
                 , ("e(/",    "ἕ")
                 , ("e(\\",   "ἓ")
                 , ("e)",     "ἐ")
                 , ("e)/",    "ἔ")
                 , ("e)\\",   "ἒ")
                 , ("e/",     "έ")
                 , ("e\\",    "ὲ")
                 , ("f",      "φ")
                 , ("g",      "γ")
                 , ("h",      "η")
                 , ("h(",     "ἡ")
                 , ("h(/",    "ἥ")
                 , ("h(/|",   "ᾕ")
                 , ("h(=",    "ἧ")
                 , ("h(=|",   "ᾗ")
                 , ("h(\\",   "ἣ")
                 , ("h(\\|",  "ᾓ")
                 , ("h(|",    "ᾑ")
                 , ("h)",     "ἠ")
                 , ("h)/",    "ἤ")
                 , ("h)/|",   "ᾔ")
                 , ("h)=",    "ἦ")
                 , ("h)=|",   "ᾖ")
                 , ("h)\\",   "ἢ")
                 , ("h)\\|",  "ᾒ")
                 , ("h)|",    "ᾐ")
                 , ("h/",     "ή")
                 , ("h/|",    "ῄ")
                 , ("h=",     "ῆ")
                 , ("h=|",    "ῇ")
                 , ("h\\",    "ὴ")
                 , ("h\\|",   "ῂ")
                 , ("h|",     "ῃ")
                 , ("i",      "ι")
                 , ("i(",     "ἱ")
                 , ("i(/",    "ἵ")
                 , ("i(=",    "ἷ")
                 , ("i(\\",   "ἳ")
                 , ("i)",     "ἰ")
                 , ("i)/",    "ἴ")
                 , ("i)=",    "ἶ")
                 , ("i)\\",   "ἲ")
                 , ("i+",     "ϊ")
                 , ("i/",     "ί")
                 , ("i/+",    "ΐ")
                 , ("i=",     "ῖ")
                 , ("i=+",    "ῗ")
                 , ("i\\",    "ὶ")
                 , ("i\\+",   "ῒ")
                 , ("j",      "ς")
                 , ("k",      "κ")
                 , ("l",      "λ")
                 , ("m",      "μ")
                 , ("n",      "ν")
                 , ("o",      "ο")
                 , ("o(",     "ὁ")
                 , ("o(/",    "ὅ")
                 , ("o(\\",   "ὃ")
                 , ("o)",     "ὀ")
                 , ("o)/",    "ὄ")
                 , ("o)\\",   "ὂ")
                 , ("o/",     "ό")
                 , ("o\\",    "ὸ")
                 , ("p",      "π")
                 , ("q",      "θ")
                 , ("r",      "ρ")
                 , ("r(",     "ῥ")
                 , ("r)",     "ῤ")
                 , ("s",      "σ")
                 , ("s1",     "σ")
                 , ("s2",     "ς")
                 , ("s3",     "ϲ")
                 , ("t",      "τ")
                 , ("u",      "υ")
                 , ("u(",     "ὑ")
                 , ("u(/",    "ὕ")
                 , ("u(=",    "ὗ")
                 , ("u(\\",   "ὓ")
                 , ("u)",     "ὐ")
                 , ("u)/",    "ὔ")
                 , ("u)=",    "ὖ")
                 , ("u)\\",   "ὒ")
                 , ("u+",     "ϋ")
                 , ("u/",     "ύ")
                 , ("u/+",    "ΰ")
                 , ("u=",     "ῦ")
                 , ("u=+",    "ῧ")
                 , ("u\\",    "ὺ")
                 , ("u\\+",   "ῢ")
                 , ("w",      "ω")
                 , ("w(",     "ὡ")
                 , ("w(/",    "ὥ")
                 , ("w(/|",   "ᾥ")
                 , ("w(=",    "ὧ")
                 , ("w(=|",   "ᾧ")
                 , ("w(\\",   "ὣ")
                 , ("w(\\|",  "ᾣ")
                 , ("w(|",    "ᾡ")
                 , ("w)",     "ὠ")
                 , ("w)/",    "ὤ")
                 , ("w)/|",   "ᾤ")
                 , ("w)=",    "ὦ")
                 , ("w)=|",   "ᾦ")
                 , ("w)\\",   "ὢ")
                 , ("w)\\|",  "ᾢ")
                 , ("w)|",    "ᾠ")
                 , ("w/",     "ώ")
                 , ("w/|",    "ῴ")
                 , ("w=",     "ῶ")
                 , ("w=|",    "ῷ")
                 , ("w\\",    "ὼ")
                 , ("w\\|",   "ῲ")
                 , ("w|",     "ῳ")
                 , ("x",      "χ")
                 , ("y",      "ψ")
                 , ("z",      "ζ")
                 ]

encode :: [Char] -> [Word8]
encode = BS.unpack . TE.encodeUtf8 . T.pack
