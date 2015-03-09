{-# LANGUAGE OverloadedStrings #-}
{-
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2045Spec
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   Network.Mail.Mime.Parser.Internal.Rfc2045Spec
-}

module Network.Mail.Mime.Parser.Internal.Rfc2045Spec ( main, spec ) where

import Test.Hspec
import Util
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2045

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2045.token" $
    it "parses hand-picked inputs correctly" $ do
      parseTest token "charset" `shouldReturn` "charset"
      parseTest token "ISO-8859-1" `shouldReturn` "ISO-8859-1"
      parseFailure token "charset="
      parseFailure token "charset?"

  describe "Rfc2045.content_type_parm" $
    it "parses hand-picked inputs correctly" $ do
      parseTest content_type_parm "charset=utf-8" `shouldReturn` Charset "utf-8"

  describe "Rfc2045.content_type" $
    it "parses hand-picked inputs correctly" $ do
      let parseTest' = parseTest content_type
      parseTest' "Content-Type: image/jpeg\r\n" `shouldReturn`
        ContentType "image" "jpeg" []
      parseTest' "Content-Type: text/plain; charset=utf-8\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8"])
      parseTest' "Content-Type: text/plain; charset = utf-8\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8"])
      parseTest' "Content-Type: text/plain; charset=utf-8; foo=bar\r\n"
        `shouldReturn` (ContentType "text" "plain" [Charset "utf-8", ContentTypeParm "foo" "bar"])

      parseTest' "Content-Type: multipart/mixed; boundary=\"_----------=_142387904494050\"\r\n" `shouldReturn`(ContentType "multipart" "mixed" [Boundary "_----------=_142387904494050"])

  describe "Rfc2045.quoted_printable" $
    it "parses hand-picked inputs correctly" $ do
      let parseTest' = parseTest quoted_printable
      parseTest' "Now's the time =\r\nfor all folk to come=\r\n to the aid of their country." `shouldReturn` "Now's the time for all folk to come to the aid of their country."
      parseTest' "Now's the time\r\nfor all folk to come=\r\n to the aid of their country.\r\n" `shouldReturn` "Now's the time\nfor all folk to come to the aid of their country.\n"
      parseTest' "Vilar=C3=B3" `shouldReturn` "Vilar\195\179"
      parseTest' "" `shouldReturn` ""
      parseTest' "<table background=3D\"cid:4c837ed463ad29c820668e835a270e8a.jpg\" width=3D\"100=\r\n%\">" `shouldReturn` "<table background=\"cid:4c837ed463ad29c820668e835a270e8a.jpg\" width=\"100%\">"
