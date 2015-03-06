{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2045
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2045, \"Multipurpose Internet Mail Extensions\",
   <https://tools.ietf.org/html/rfc2045>
-}

module Network.Mail.Mime.Parser.Internal.Rfc2045 where


import Control.Applicative (many, pure, (<$>), (<*>), (<*), (*>), (<|>))
import Data.Char (isAscii, ord)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2822
import Prelude hiding (takeWhile)

attribute :: Parser ByteString
attribute = fmap sToLower token

composite_type :: Parser ByteString
composite_type = fmap sToLower
               $ stringCI "message"
             <|> stringCI "multipart"
             <|> extension_token

discrete_type :: Parser ByteString
discrete_type = fmap sToLower
              $ stringCI "text"
            <|> stringCI "image"
            <|> stringCI "audio"
            <|> stringCI "video"
            <|> stringCI "application"
            <|> extension_token

content_type :: Parser Field
content_type
  = header "Content-Type" $
      ContentType <$> type_
                  <*> ("/"*>subtype)
                  <*> many (trim ";" *> content_type_parm)

content_description :: Parser Field
content_description
  = ContentDescription <$> header "Content-Description" atextstring

content_encoding :: Parser Field
content_encoding
  = ContentTransferEncoding <$> header "Content-Transfer-Encoding" mechanism

mechanism :: Parser Encoding
mechanism = stringCI "7bit"             *> pure Binary7Bit
       <|>  stringCI "8bit"             *> pure Binary8Bit
       <|>  stringCI "binary"           *> pure Binary
       <|>  stringCI "quoted-printable" *> pure QuotedPrintable
       <|>  stringCI "base64"           *> pure Base64
       <|> (Encoding <$> x_token)
       <|> (Encoding <$> ietf_token)

content_type_parm :: Parser ContentTypeParm
content_type_parm = do
  (attr, val) <- parameter
  return $ case attr of
    "boundary" -> Boundary val
    "name"     -> Name val
    "charset"  -> Charset val
    _          -> ContentTypeParm attr val

someparameter :: Parser ByteString -> Parser a -> Parser (ByteString, a)
someparameter a v = (,) <$> a <*> ((trim "=") *> v)

parameter :: Parser (ByteString, ByteString)
parameter = someparameter attribute value

value :: Parser ByteString
value = token <|> quoted_string_text_no_quotes

type_ :: Parser ByteString
type_ = discrete_type <|> composite_type

subtype :: Parser ByteString
subtype = extension_token <|> iana_token

iana_token :: Parser ByteString 
iana_token = token

ietf_token :: Parser ByteString 
ietf_token = token

extension_token :: Parser ByteString
extension_token = ietf_token <|> x_token

x_token :: Parser ByteString
x_token = fmap ("x-"<>) (stringCI "x-" *> token)

token :: Parser ByteString
token = takeWhile1 (\c -> isAscii c
                       && not (isSpace c || isNoWsCtl c || c `elem` tspecials))


tspecials :: [Char]
tspecials = "()<>@,;:\\\"/[]?="

mime_extension_field :: Parser Field
mime_extension_field
    = MimeExtension
  <$> between (stringCI "Content-") ":" (takeWhile1 (/=':'))
  <*> (unstructured <* crlf)

entity_headers :: Parser [Field]
entity_headers = many entity_header

entity_header :: Parser Field
entity_header
    = content_type
  <|> content_encoding
  <|> content_description
  <|> mime_extension_field

mime_message_headers :: Parser [Field]
mime_message_headers = many mime_message_header

mime_message_header :: Parser Field
mime_message_header = entity_header <|> rfc2822_field <|> mime_version


mime_version :: Parser Field
mime_version
  = header "Mime-Version" (MimeVersion <$> readIntN 1 <*> ("." *> readIntN 1))


quoted_printable :: Parser ByteString
quoted_printable = do
  l <- qp_line
  ls <- many ((S.cons '\n') <$> (crlf *> qp_line))
  return $ l <> S.concat ls


qp_line :: Parser ByteString
qp_line = do
  ls <- many (qp_segment <* transport_padding <* crlf)
  l <- qp_part <* transport_padding
  return $ S.concat ls <> l

qp_part :: Parser ByteString
qp_part = qp_section

qp_segment :: Parser ByteString
qp_segment = (<>) <$> qp_section <*> transport_padding <* "="

qp_section :: Parser ByteString
qp_section = do
  ls <- many (transport_padding1 <|> ptextstring)
  l <- option "" ptextstring
  return $ S.concat ls <> l

ptextstring :: Parser ByteString
ptextstring = S.concat <$> many1 (hex_octet <|> takeWhile1 isSafeChar)

isSafeChar :: Char -> Bool
isSafeChar c = (ord c >= 33 && ord c <= 60) || (ord c >= 62 && ord c <= 126)

transport_padding :: Parser ByteString
transport_padding = takeWhile isHorizontalSpace

transport_padding1 :: Parser ByteString
transport_padding1 = takeWhile1 isHorizontalSpace
