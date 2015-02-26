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


import Control.Applicative ((<$>), (<*), (*>), (<|>), pure, many)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2045
import Network.Mail.Mime.Parser.Internal.Rfc2822
import Network.Mail.Mime.Parser.Internal.Rfc2183
import Network.Mail.Mime.Parser.Internal.Unicode
import Prelude hiding (takeWhile)

multipart_body :: ByteString -> Parser Body
multipart_body boundary = named "multipart_body" $ do
  let dash_boundary = "--" *> string boundary *> pure () <?> "dash_boundary"
      delimiter = crlf *> dash_boundary *> pure ()       <?> "delimiter"
      close_delimiter = delimiter *> "--" *> pure ()     <?> "close_delimiter"
      sep = (close_delimiter <* transport_padding)
        <|> (delimiter <* transport_padding >> crlf >> pure ())
        <?> "part sep"
  pr <- option "" (preamble <* crlf) <?> "preamble"
  dash_boundary <* transport_padding <* crlf <?> "initial boundary"
  parts <- many1 (body_part sep) <?> "parts"
  ep <- option "" (crlf *> epilogue) <?> "epilogue"
  return $ MultipartBody pr parts ep

mime_part_headers :: Parser [Field]
mime_part_headers = named "mime_part_headers" $ many mime_part_header

mime_part_header :: Parser Field
mime_part_header = content_disposition <|> entity_header <|> rfc2822_field

body_part :: Parser () -> Parser Part
body_part sep = named "body_part" $ do
  hs <- mime_part_headers <?> "body_part headers"
  optional crlf
  bd <- case getContentType hs of
    ContentType "multipart" _ ps ->
      case getBoundary ps of
        Just b -> multipart_body b <* sep <?> "nested multipart_body"
        _      -> fail "multipart content with no boundary"
    ContentType "text" _ ps -> do
      bd <- case getEncoding hs of
                 QuotedPrintable -> qp_body sep
                 Base64          -> do bd <- fmap B64.decode (binary_body sep)
                                       either fail return bd
                 _               -> binary_text_body sep
      either fail (return . TextBody) $ toUnicode (S.unpack (getCharset ps)) bd
    _ -> do
      bd <- case getEncoding hs of
              QuotedPrintable -> qp_body sep
              Base64          -> do bd <- fmap B64.decode (binary_body sep)
                                    either fail return bd
              _               -> binary_body sep
      return $ BinaryBody bd
  return $ Part hs bd

binary_body :: Parser () -> Parser ByteString
binary_body sep = S.concat <$> manyTill takeLine sep

binary_text_body :: Parser () -> Parser ByteString
binary_text_body sep = S.unlines <$> manyTill takeLine sep

takeLine :: Parser ByteString
takeLine = takeWhile1 (/='\r') <|> ("\r\n" *> pure "")


qp_body :: Parser () -> Parser ByteString
qp_body sep = named "qp_body" $ S.concat <$> manyTill quoted_printable sep

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
