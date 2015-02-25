{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Module      :  Network.Mail.Mime.ParserSpec
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown
-}

module Network.Mail.Mime.ParserSpec ( main, spec ) where

import Test.Hspec
import Control.Applicative ((<*))
import Control.Lens
import Control.Monad (forM_)
import Data.ByteString.Char8 (ByteString, readFile)
import Data.Attoparsec.ByteString.Char8 (parseOnly, endOfInput)
import Network.Mail.Mime.Parser
import Prelude hiding (readFile)

main :: IO ()
main = hspec spec

spec :: Spec
spec = sequence_ . map fixture_spec $ [
    fixture {
        mailId          = "m0001"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail avec fichier attaché de 1ko"
          ]
      }
  , fixture {
        mailId          = "m0002"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail avec fichier attaché de 3ko"
          ]
      }
  ]

fixture_spec :: Fixture -> Spec
fixture_spec Fixture{..} = describe mailId $ do
  email <- runIO $ readFile $ "test/fixtures/" ++ mailId
  let tryParse act
        = case parseOnly (message <* endOfInput) email of
            Left err -> expectationFailure $
                          "Could not parse " ++ mailId ++ ": " ++ err
            Right p -> act p
  forM_ expectedHeaders $ \h -> 
    it ("has expected header " ++ show h) $ tryParse $ \parsed ->
      (h `elem` parsed^.msgHeaders) `shouldBe` True


data Fixture
  = Fixture {
        mailId            :: String
      , expectedHeaders   :: [Field]
      , textMatches       :: [Match]
      , htmlMatches       :: [Match]
      , attachmentMatches :: [AttachmentMatch]
  } deriving (Eq, Show)

fixture :: Fixture
fixture = Fixture "" [] [] [] []

data Match
  = Match ByteString
  | Count Int ByteString
  deriving (Eq, Show)

data AttachmentMatch
  = AttachmentMatch {
        filename    :: ByteString
      , size        :: Integer
      , fileMatches :: [Match]
  } deriving (Eq, Show)
