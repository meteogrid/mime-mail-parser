{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Util
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides accessors to Message parts
-}
module Network.Mail.Mime.Parser.Util (
    getContentType
  , getAttachments
  , getPlainText
  , getHtmlText
  , getFilename
  , getContentDisposition
  , getFrom
  , getTo
  , getSubject
) where

import Data.Text (Text)
import Control.Lens
import Control.Monad (join)
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Common (
    getAttachments
  , getTextBody
  , getContentType
  , getContentDisposition
  , getFilename
  , firstJust
  )


getFrom :: Message -> Maybe [NameAddr]
getFrom = firstJust . map (^?_From) . (^.msgHeaders)

getTo :: Message -> Maybe [NameAddr]
getTo = firstJust . map (^?_To) . (^.msgHeaders)

getSubject :: Message -> Maybe Text
getSubject = firstJust . map (^?_Subject) . (^.msgHeaders)


getHtmlText :: Message -> Maybe Text
getHtmlText  = join . fmap (^?_TextBody) . getTextBody "html"

getPlainText :: Message -> Maybe Text
getPlainText = join . fmap (^?_TextBody) . getTextBody "plain"
