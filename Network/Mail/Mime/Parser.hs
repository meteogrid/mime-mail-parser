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
  module ReExport
) where

import Network.Mail.Mime.Parser.Types as ReExport
import Network.Mail.Mime.Parser.Internal.Rfc2822 as ReExport
