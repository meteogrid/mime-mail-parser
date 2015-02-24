{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


import Control.Applicative (many, pure, (<$>), (<*>), (<*), (*>), (<|>))
import Control.Exception (try, evaluate)
import Data.Char (chr, toLower)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.ICU.Error as ICU
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common hiding (try)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (takeWhile)

hex_octet :: Parser ByteString
hex_octet = S.singleton . chr <$> ("=" *> hexadecimal)

encoded_word :: Parser Text
encoded_word = between "=?" "?=" $ do
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

bEncodedText :: String -> Parser Text
bEncodedText charset = do
  s <- takeWhile (/='?')
  case B64.decode s of
    Left e   -> fail e
    Right s' -> either fail return . toUnicode charset $ s'


toUnicode :: String -> ByteString -> Either String Text
toUnicode encoding str = unsafePerformIO $ do
  eConverter <- tryICU $ ICU.open encoding (Just True)
  return $ case eConverter of
    Left _           -> Left $ "Invalid encoding: " ++ encoding
    Right converter  -> Right $ ICU.toUnicode converter str

tryICU :: IO a -> IO (Either ICU.ICUError a)
tryICU = try
