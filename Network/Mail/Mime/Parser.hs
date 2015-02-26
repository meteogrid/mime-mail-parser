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
  , module Network.Mail.Mime.Parser.Types
) where

import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2045 (mime_message_headers)
import Network.Mail.Mime.Parser.Internal.Rfc2046 (message, body)
