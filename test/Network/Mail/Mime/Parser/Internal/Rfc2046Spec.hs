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
import Network.Mail.Mime.Parser.Internal.Rfc2046
import Network.Mail.Mime.Parser.Internal.Common (endOfInput)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2046.binary_body" $ do
    let parseTest' = parseTest $ binary_body endOfInput
    it "parses hand-picked inputs correctly" $ do
      parseTest' "foo\r\nbar\r\n" `shouldReturn` "foobar"
      parseTest' "\r\nfoo\r\nbar\r\n" `shouldReturn` "foobar"
      parseTest' "\r\n\r\n" `shouldReturn` ""
      parseTest' "" `shouldReturn` ""

  describe "Rfc2046.binary_text_body" $ do
    let parseTest' = parseTest $ binary_text_body endOfInput
    it "parses hand-picked inputs correctly" $ do
      parseTest' "foo\r\nbar\r\n" `shouldReturn` "foo\nbar\n"
      parseTest' "\r\n\r\n" `shouldReturn` "\n\n"
      parseTest' "" `shouldReturn` ""
