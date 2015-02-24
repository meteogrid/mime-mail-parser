{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2183
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2183, \"Communicating Presentation Information in Internet Messages:
   The Content-Disposition Header Field\"
   <https://tools.ietf.org/html/rfc2183>
-}

module Network.Mail.Mime.Parser.Internal.Rfc2183 where


import Control.Applicative (many, pure, (<$>), (<*>), (<*), (*>), (<|>))
import Data.Char (isAscii, toLower)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2822
import Network.Mail.Mime.Parser.Internal.Rfc2045

content_disposition :: Parser Field
content_disposition
  = header "Content-Disposition" $
      ContentDisposition <$> disposition_type
                         <*> many (trim ";" *> content_disposition_parm)

disposition_type :: Parser ContentDispositionType
disposition_type
    = (stringCI "inline"       *> pure Inline)
  <|> (stringCI "attachment"   *> pure Attachment)
  <|> (ContentDispositionType <$> extension_token)


content_disposition_parm :: Parser ContentDispositionParm
content_disposition_parm
    = filename_parm
  <|> creation_date_parm
  <|> modification_date_parm
  <|> read_date_parm
  <|> size_parm
  <|> uncurry ContentDispositionParm <$> parameter

filename_parm :: Parser ContentDispositionParm
filename_parm
  = Filename . snd <$> someparameter (stringCI "filename") value

creation_date_parm :: Parser ContentDispositionParm
creation_date_parm
    = CreationDate . snd
  <$> someparameter (stringCI "creation-date") (quoted date_time)

modification_date_parm :: Parser ContentDispositionParm
modification_date_parm
    = ModificationDate . snd
  <$> someparameter (stringCI "modification-date") (quoted date_time)

read_date_parm :: Parser ContentDispositionParm
read_date_parm
    = ReadDate . snd
  <$> someparameter (stringCI "read-date") (quoted date_time)

size_parm :: Parser ContentDispositionParm
size_parm
    = Size . snd
  <$> someparameter (stringCI "size") decimal

quoted :: Parser a -> Parser a
quoted = between "\"" "\""
