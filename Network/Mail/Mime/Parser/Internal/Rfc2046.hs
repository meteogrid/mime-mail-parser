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


import Control.Applicative (pure, many, (<$>), (<*>), (<*), (*>), (<|>))
import Data.Char (ord)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2045
import Network.Mail.Mime.Parser.Internal.Rfc2822
import Prelude hiding (takeWhile)

multipart_body :: ByteString -> Parser MultipartBody
multipart_body boundary = do
  let dash_boundary = "--" *> string boundary *> pure ()
      delimiter = crlf *> dash_boundary *> pure ()
      close_delimiter = delimiter *> "--" *> pure ()
      sep = (delimiter >> transport_padding >> crlf >> pure ())
        <|> (close_delimiter >> transport_padding)
      body_part = Part <$> mime_part_headers <*> (crlf *> part_body)
      part_body = fmap S.pack (manyTill anyChar sep)
  pr <- option "" (preamble <* crlf)
  dash_boundary >> transport_padding <* crlf
  parts <- many1 body_part
  ep <- option "" (crlf <* epilogue)
  return $ MultipartBody pr parts ep

transport_padding :: Parser ()
transport_padding = takeWhile isHorizontalSpace *> return ()


preamble :: Parser ByteString
preamble = discard_text

epilogue :: Parser ByteString
epilogue = discard_text

discard_text :: Parser ByteString
discard_text = takeWhile (\c -> isText c || c=='\r' || c=='\n')
