{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2045
   Copyright   :  (c) 2015 Alberto Valverde
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
import Data.Char (isAscii, toLower)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2822

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
      ContentType <$> type_ <*> ("/"*>subtype) <*> many (trim ";" *> parameter)

content_description :: Parser Field
content_description
  = ContentDescription <$> header "Content-Description" atextstring

content_encoding :: Parser Field
content_encoding
  = ContentTransferEncoding <$> header "Content-Transfer-Encoding" mechanism

mechanism :: Parser Encoding
mechanism = "7bit"            *> pure Binary7Bit
       <|> "8bit"             *> pure Binary8Bit
       <|> "binary"           *> pure Binary
       <|> "quoted-printable" *> pure QuotedPrintable
       <|> (OtherEncoding <$> x_token)
       <|> (OtherEncoding <$> ietf_token)

parameter :: Parser Parameter
parameter = do
  attr <- attribute
  val  <- (trim "=") *> value
  return $ case attr of
    "boundary" -> Boundary val
    "name"     -> Name val
    "charset"  -> Charset val
    _          -> OtherParameter attr val

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

hex_octet :: Parser ByteString
hex_octet = do
  _ <- "="
  a <- hex_digit
  b <- hex_digit
  return $ S.pack ['=', a, b]

hex_digit :: Parser Char
hex_digit = digit <|> satisfy (\c -> c>='A' && c<= 'F')

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

mime_part_headers :: Parser [Field]
mime_part_headers = many mime_part_header

mime_part_header :: Parser Field
mime_part_header = entity_header <|> rfc2822_field

mime_version :: Parser Field
mime_version
  = header "Mime-Version" (MimeVersion <$> readIntN 1 <*> ("." *> readIntN 1))

--
-- Utils
--
sToLower :: ByteString -> ByteString
sToLower = S.map toLower
