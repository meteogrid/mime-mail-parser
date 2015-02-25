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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rfc2047.hex_octet" $
    it "parses hand-picked inputs correctly" $ do
      parseTest hex_octet "=20" `shouldReturn` " "

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
    
