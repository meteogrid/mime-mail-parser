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
  , mime_message_headers
  , body
  , parseMessage
  , parseMessage'
  , module Network.Mail.Mime.Parser.Types
  , module Network.Mail.Mime.Parser.Util
) where

import Control.Applicative ((<*))
import Data.Attoparsec.ByteString.Char8 (parseOnly, endOfInput)
import Data.ByteString.Char8 (ByteString)
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Util
import Network.Mail.Mime.Parser.Internal.Rfc2045 (mime_message_headers)
import Network.Mail.Mime.Parser.Internal.Rfc2046 (message, body)
import Network.Mail.Mime.Parser.Internal.Common (ensureCRLFeols)

-- handles both \r\n EOLs and \n EOLs
parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly (message <* endOfInput) . ensureCRLFeols

-- EOLs must be \r\n
parseMessage' :: ByteString -> Either String Message
parseMessage' = parseOnly (message <* endOfInput)
