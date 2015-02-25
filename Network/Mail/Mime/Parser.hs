{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      :  Network.Mail.Mime.Parser
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

 -}

module Network.Mail.Mime.Parser (
    message
  , module Network.Mail.Mime.Parser.Types
) where

import Control.Applicative (pure, (<$>), (*>))
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Internal.Rfc2822 (body)
import Network.Mail.Mime.Parser.Internal.Rfc2234 (crlf)
import Network.Mail.Mime.Parser.Internal.Rfc2045 (mime_message_headers)
import Network.Mail.Mime.Parser.Internal.Rfc2046 (multipart_body)

message :: Parser Message
message = do
  optional envelope
  f <- mime_message_headers
  optional crlf
  b <- case getBoundary f of
    Just b -> option (MultipartBody "" [] "") (multipart_body b)
    _      -> BinaryBody <$> option "" body
  return (Message f b)

envelope :: Parser ()
envelope = "From " *> takeWhile1 (/='\r') *> crlf *> pure ()
