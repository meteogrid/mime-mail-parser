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
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString, readFile)
import qualified Data.ByteString.Char8 as S
import Data.List (find)
import Network.Mail.Mime.Parser
import Network.Mail.Mime.Parser.Internal.Common
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
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = "attach01"
            , size           = 2
            , fileMatches    = [Count 1 "a"]
            , contentType    = "application"
            , contentSubtype = "octet-stream"
            }
          ]
                        
      }
  , fixture {
        mailId          = "m0002"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail avec fichier attaché de 3ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = "attach02"
            , size           = 2229
            , fileMatches    = [Count 8 "Lorem ipsum"]
            , contentType    = "application"
            , contentSubtype = "octet-stream"
            }
          ]
      }
  , fixture {
        mailId          = "m0003"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail de 14 Ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = "attach03"
            , size           = 13369
            , fileMatches    = [Count 48 "dolor sit amet"]
            , contentType    = "application"
            , contentSubtype = "octet-stream"
            }
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

      textMatchSpec stype m = it (show stype ++ " body matches " ++ show m) $
        tryParse $ \parsed -> do
          case textBody stype parsed of
            Just t ->
              case m of
                Match s   -> t `shouldBe` s
                Count n s -> T.count s t `shouldBe` n
            _ -> expectationFailure $ "email has no "++ show stype++" body"

  -- Check for expected headers
  forM_ expectedHeaders $ \h -> 
    it ("has expected header " ++ show h) $ tryParse $ \parsed ->
      (h `elem` parsed^.msgHeaders) `shouldBe` True
  
  -- Check for Matches on text body
  forM_ textMatches $ textMatchSpec "plain"
 
  -- Check for Matches on html body
  forM_ htmlMatches $ textMatchSpec "html"

  -- Check for Matches on html body
  forM_ (zip [0..] attachmentMatches) $ \(i,a) ->
    it ("attachment " ++ show i ++ " matches " ++ show a) $
      tryParse $ \parsed -> do
        let atch = getAttachments (parsed^.msgBody) !! i
            size' = case atch^.partBody of
                      TextBody   t -> Just . fromIntegral $ T.length t
                      BinaryBody s -> Just . fromIntegral $ S.length s
                      _            -> Nothing 
        getFilename (atch^.partHeaders) `shouldBe` Just (filename a)
        size' `shouldBe` Just (size a)
        let ContentType ct cst _ = getContentType (atch^.partHeaders)
        ct `shouldBe` contentType a
        cst `shouldBe` contentSubtype a


textBody :: ByteString -> Message -> Maybe Text
textBody subtype m
  = case m^.msgBody of
      TextBody t -> Just t
      MultipartBody _ (x:_) _ ->
        case (getContentType (x^.partHeaders), x^.partBody) of
          (ContentType "multipart" "alternative" _, MultipartBody _ ps _) ->
            fmap (\p -> p^.partBody._TextBody) (find isTextPart ps)
          (ContentType "text" st _, TextBody t) | st==subtype -> Just t
          _ -> Nothing
      _ -> Nothing

  where
    isTextPart :: Part -> Bool
    isTextPart p = case getContentType (p^.partHeaders) of
                     ContentType "text" st _ | st==subtype -> True
                     _ -> False
                    
            

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
  = Match Text
  | Count Int Text
  deriving (Eq, Show)

data AttachmentMatch
  = AttachmentMatch {
        filename       :: ByteString
      , size           :: Integer
      , fileMatches    :: [Match]
      , contentType    :: ByteString
      , contentSubtype :: ByteString
  } deriving (Eq, Show)
