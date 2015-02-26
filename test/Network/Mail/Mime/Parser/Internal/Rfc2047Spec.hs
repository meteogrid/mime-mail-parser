{-# LANGUAGE OverloadedStrings #-}
{-
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2047Spec
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   Network.Mail.Mime.Parser.Internal.Rfc2047Spec
-}

module Network.Mail.Mime.Parser.Internal.Rfc2047Spec ( main, spec ) where

import Test.Hspec
import Util
import Network.Mail.Mime.Parser.Internal.Rfc2047
import Network.Mail.Mime.Parser.Internal.Common (hex_octet)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2047.hex_octet" $
    it "parses hand-picked inputs correctly" $ do
      parseTest hex_octet "=20" `shouldReturn` " "
      parseTest hex_octet "=5F" `shouldReturn` "_"
      parseFailure hex_octet "=5"

  describe "Rfc2047.encoded_word" $ do
    let parseTest'    = parseTest encoded_word
        parseFailure' = parseFailure encoded_word
    it "parses hand-picked inputs correctly" $ do
      parseTest' "=?ISO-8859-1?Q?Mail_avec_fichier_attach=E9_de_3ko?="
        `shouldReturn` "Mail avec fichier attaché de 3ko"
      parseTest' "=?ISO-8859-1?Q??=" `shouldReturn` ""
      parseTest' "=?ks_c_5601-1987?B?u+fB+C5KUEc=?=" `shouldReturn` "사진.JPG"
      parseTest' "=?ks_c_5601-1987?B??=" `shouldReturn` ""
      parseFailure' "=?ks_c_5601-1987?A?bar?="
      parseFailure' "=?foo?B?bar?="
      parseTest' "=?iso-8859-1?Q?50032266_CAR_11=5FMNPA00A01=5F9PTX=5FH00_ATT_N=B0_1467829.?="
        `shouldReturn` "50032266 CAR 11_MNPA00A01_9PTX_H00 ATT N° 1467829."
