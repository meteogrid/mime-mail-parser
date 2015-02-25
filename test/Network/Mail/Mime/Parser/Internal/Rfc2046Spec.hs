{-# LANGUAGE OverloadedStrings #-}
{-
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2046Spec
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   Network.Mail.Mime.Parser.Internal.Rfc2046Spec
-}

module Network.Mail.Mime.Parser.Internal.Rfc2046Spec ( main, spec ) where

import Test.Hspec
import Util
import Data.ByteString.Char8 (ByteString)
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2046

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2046.discard_text" $
    it "parses hand-picked inputs correctly" $ do
      parseIdemTest discard_text ""
      parseIdemTest discard_text "foo bar\r\nzoo car"
      parseIdemTest discard_text "foo bar\r\nzoo car\r\n"
