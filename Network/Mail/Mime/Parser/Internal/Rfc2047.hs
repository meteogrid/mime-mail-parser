{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2047
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2047, \"Multipurpose Internet Mail Extensions\",
   <https://tools.ietf.org/html/rfc2047>
-}

module Network.Mail.Mime.Parser.Internal.Rfc2047 where


import Control.Applicative (many, (<$>), (*>), (<|>))
import Data.Char (chr, toLower)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as S
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Internal.Unicode
import Prelude hiding (takeWhile)

encoded_word :: Parser Text
encoded_word = named "encoded_word" . between "=?" "?=" $ do
  charset <- fmap S.unpack $ takeWhile1 (/='?')
  encoding <- between "?" "?" (oneOf "qQbB")
  case toLower encoding of
    'q' -> qEncodedText charset
    'b' -> bEncodedText charset
    _   -> fail ("Invalid encoding: " ++ show encoding)

qEncodedText :: String -> Parser Text
qEncodedText charset
    = many (hex_octet <|> takeWhile1 (\c -> c/='=' && c/='?'))
  >>= either fail (return . T.replace "_" " ") . toUnicode charset . S.concat

-- Copied from Rfc2045 to prevent circular dependency
hex_octet :: Parser ByteString
hex_octet = S.singleton . chr <$> ("=" *> hexadecimal)

bEncodedText :: String -> Parser Text
bEncodedText charset = do
  s <- takeWhile (/='?')
  case B64.decode s of
    Left e   -> fail e
    Right s' -> either fail return . toUnicode charset $ s'
