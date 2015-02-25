{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2046
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2045, \"Multipurpose Internet Mail Extensions\",
   <https://tools.ietf.org/html/rfc2046>
-}

module Network.Mail.Mime.Parser.Internal.Rfc2046 where


import Control.Applicative ((<$>), (<*), (*>), (<|>), pure)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2045
import Network.Mail.Mime.Parser.Internal.Rfc2822
import Prelude hiding (takeWhile)

multipart_body :: ByteString -> Parser Body
multipart_body boundary = do
  let dash_boundary = "--" *> string boundary *> pure ()
      delimiter = crlf *> dash_boundary *> pure ()
      close_delimiter = delimiter *> "--" *> pure ()
      sep = (close_delimiter <* transport_padding)
        <|> (delimiter <* transport_padding >> crlf >> pure ())
  pr <- option "" (preamble <* crlf)
  dash_boundary <* transport_padding <* crlf
  parts <- many1 (body_part sep)
  ep <- option "" (crlf *> epilogue)
  return $ MultipartBody pr parts ep

body_part :: Parser () -> Parser Part
body_part sep = do
  hs <- mime_part_headers
  optional crlf
  bd <- case getBoundary hs of
    Just b -> multipart_body b <* sep
    _      -> binary_body sep
  return $ Part hs bd

binary_body :: Parser () -> Parser Body
binary_body sep = BinaryBody . S.intercalate "\r"
              <$> manyTill (takeWhile1 (/='\r') <|> "\r") sep

preamble :: Parser ByteString
preamble = discard_text

epilogue :: Parser ByteString
epilogue = discard_text

discard_text :: Parser ByteString
discard_text = do
  ch <- peekChar
  case ch of
    Just '-' -> fail "discard_text"
    _        -> takeWhile (\c -> isText c || c=='\r' || c=='\n')
