{-# LANGUAGE OverloadedStrings #-}
{-
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2183Spec
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   Network.Mail.Mime.Parser.Internal.Rfc2183Spec
-}

module Network.Mail.Mime.Parser.Internal.Rfc2183Spec ( main, spec ) where

import System.Time (CalendarTime(..), Month(..), Day(..))
import Test.Hspec
import Util
import Data.ByteString.Char8 (ByteString)
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2183

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2183.content_disposition" $ do
    let parseTest' = parseTest content_disposition
    it "parses hand-picked inputs correctly" $ do
      parseTest' "Content-Disposition: inline\r\n"
        `shouldReturn` ContentDisposition Inline []
      parseTest' "Content-Disposition: attachment; filename=\"FooBar\"\r\n"
        `shouldReturn` ContentDisposition Attachment [Filename "FooBar"]
      parseTest' "Content-Disposition: attachment; filename=FooBar\r\n"
        `shouldReturn` ContentDisposition Attachment [Filename "FooBar"]
      parseTest' "Content-Disposition: attachment; filename=FooBar; modification-date=\"Fri, 14 Mar 2014 09:51:55 GMT\"\r\n"
        `shouldReturn` ContentDisposition Attachment [Filename "FooBar", ModificationDate (CalendarTime {ctYear = 2014, ctMonth = March, ctDay = 14, ctHour = 9, ctMin = 51, ctSec = 55, ctPicosec = 0, ctWDay = Friday, ctYDay = 0, ctTZName = "", ctTZ = 0, ctIsDST = False})]
      parseTest' "Content-Disposition: attachment; filename=FooBar; modification-date=\"Fri, 14 Mar 2014 09:51:55 +0100\"\r\n"
        `shouldReturn` ContentDisposition Attachment [Filename "FooBar", ModificationDate (CalendarTime {ctYear = 2014, ctMonth = March, ctDay = 14, ctHour = 9, ctMin = 51, ctSec = 55, ctPicosec = 0, ctWDay = Friday, ctYDay = 0, ctTZName = "", ctTZ = 3600, ctIsDST = False})]
