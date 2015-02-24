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


import Control.Applicative (pure, (<$>), (<*>), (<*), (*>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2045
import Network.Mail.Mime.Parser.Internal.Rfc2822

multipart_message :: Parser Message
multipart_message = do
  f <- mime_message_headers
  b <- option empty_multipart_body (crlf *> multipart_body undefined)
  return (MultipartMessage f b)

empty_multipart_body :: MultipartBody
empty_multipart_body = MultipartBody "" [] ""

multipart_body :: ByteString -> Parser MultipartBody
multipart_body boundary = do
  let dash_boundary   = "--" *> string boundary *> pure ()
      delimiter       = crlf *> dash_boundary *> pure ()
      close_delimiter = delimiter *> "--" *> pure ()
  p <- option "" (preamble <* crlf)
  dash_boundary >> transport_padding <* crlf
  parts <- body_part `sepBy1` (delimiter >> transport_padding >> crlf)
  close_delimiter >> transport_padding
  e <- option "" (crlf <* epilogue)
  return $ MultipartBody p parts e

transport_padding :: Parser ()
transport_padding = option "" lwsp *> return ()

body_part :: Parser Part
body_part
    = Part
  <$> mime_part_headers
  <*> option "" (
        crlf *> (fmap (S.intercalate "\r\n") (takeWhile1 (/='\r') `sepBy` crlf))
        )

preamble :: Parser ByteString
preamble = discard_text

epilogue :: Parser ByteString
epilogue = discard_text

discard_text :: Parser ByteString
discard_text = do
  ls <- option "" atextstring `sepBy` crlf
  l <- option "" atextstring
  return $ S.concat ls <> l
