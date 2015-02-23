{-# LANGUAGE OverloadedStrings #-}
{-
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2822Spec
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   Network.Mail.Mime.Parser.Internal.Rfc2045Spec
-}

module Network.Mail.Mime.Parser.Internal.Rfc2045Spec ( main, spec ) where

import Test.Hspec
import Util
import Data.ByteString.Char8 (ByteString)
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2045

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2045.hex_octet" $
    it "parses hand-picked inputs correctly" $ do
      parseTest hex_octet "=0A" `shouldReturn` "=0A"

  describe "Rfc2045.token" $
    it "parses hand-picked inputs correctly" $ do
      parseTest token "charset" `shouldReturn` "charset"
      parseFailure token "charset="

  describe "Rfc2045.parameter" $
    it "parses hand-picked inputs correctly" $ do
      parseTest parameter "charset=utf-8" `shouldReturn` Charset "utf-8"

  describe "Rfc2045.content_type" $
    it "parses hand-picked inputs correctly" $ do
      parseTest content_type "Content-Type: image/jpeg\r\n" `shouldReturn`
        ContentType "image" "jpeg" []
      parseTest content_type "Content-Type: text/plain; charset=utf-8\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8"])
      parseTest content_type "Content-Type: text/plain; charset = utf-8\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8"])
      parseTest content_type "Content-Type: text/plain; charset=utf-8; foo=bar\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8", OtherParameter "foo" "bar"])

      parseTest content_type "Content-Type: multipart/mixed; boundary=\"_----------=_142387904494050\"\r\n" `shouldReturn`(ContentType "multipart" "mixed" [Boundary "_----------=_142387904494050"])
