{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-
   Module      :  Network.Mail.Mime.ParserSpec
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This is a port of the test fixture from
   https://github.com/eXorus/php-mime-mail-parser/blob/master/test/ParserTest.php
-}

module Network.Mail.Mime.ParserSpec ( main, spec ) where

import Test.Hspec
import Control.Applicative ((<*))
import Control.Lens
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString, readFile)
import qualified Data.ByteString.Char8 as S
import Data.List (find)
import Network.Mail.Mime.Parser
import Network.Mail.Mime.Parser.Internal.Common
import Prelude hiding (readFile)
import Debug.Trace (traceShow)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . sequence_ . map fixture_spec $ [
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
              filename           = "attach01"
            , size               = 2
            , fileMatches        = [BinCount 1 "a"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
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
              filename           = "attach02"
            , size               = 2229
            , fileMatches        = [BinCount 8 "Lorem ipsum"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
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
              filename           = "attach03"
            , size               = 13369
            , fileMatches        = [BinCount 48 "dolor sit amet"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0004"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail de 800ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename           = "attach04"
            , size               = 817938
            , fileMatches        = [BinCount 242 "Phasellus scelerisque"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0005"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail de 1500 Ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename           = "attach05"
            , size               = 1635877
            , fileMatches        = [BinCount 484 "Aenean ultrices"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0006"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail de 3 196 Ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename           = "attach06"
            , size               = 3271754
            , fileMatches        = [BinCount 968 "lectus ac leo ullamcorper"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0007"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail avec fichier attaché de 3ko"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename           = "attach02"
            , size               = 2229
            , fileMatches        = [BinCount 4 "facilisis"]
            , contentType        = "application"
            , contentSubtype     = "octet-stream"
            , contentDisposition = Nothing
            }
          ]
      }
  , fixture {
        mailId          = "m0008"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr (Just "Name") "name@company2.com")]
          , Subject "Testing MIME E-mail composing with cid"
          ]
      , textMatches = [Count 1 "Please use an HTML capable mail program to read"]
      , htmlMatches = [Count 1 "<center><h1>Testing MIME E-mail composing with cid</h1></center>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = "logo.jpg"
            , size           = 2695
            , fileMatches    = []
            , contentType    = "image"
            , contentSubtype = "gif"
            , contentDisposition = Just Inline
            }
          ]
      }
  ]

parseIncrementally :: Parser a -> ByteString -> Either String a
parseIncrementally p bs = go ls r 0
  where
    (l:ls) = (++[""]) . map (<>"\n") . S.lines $ bs 
    r      = parse p l
    go [] r' _
      = case r' of
          Done _ a    -> Right a
          Partial _   -> Left "Partial input on last line"
          Fail i es e -> Left $ concat [ "Parser failed on last line: "
                                       , show (S.take 100 i), ";"
                                       , show e, "; "
                                       , show es, "; "]
    go (x:xs) r' n
      = case r' of
          Done _ a    -> Right a
          Partial f   -> go xs (f x) (n+1)
          Fail i es e -> Left $ concat [ "Parser failed on line ", show n, ":"
                                       , show (S.take 100 i), ";"
                                       , show e, "; "
                                       , show es, "; "]
                
                        
      

fixture_spec :: Fixture -> Spec
fixture_spec Fixture{..} = parallel $ describe mailId $ do
  email <- runIO $ readFile $ "test/fixtures/" ++ mailId
  let tryParse act
        = case parseIncrementally message email of
            Left err -> expectationFailure $
                          "Could not parse " ++ mailId ++ ": " ++ err
            Right p -> act p

      textMatchSpec stype m = it (show stype ++ " body matches " ++ show m) $
        tryParse $ \parsed -> do
          case textBody stype parsed of
            Just b -> checkMatch b m
            _      -> expectationFailure $ "email has no "++ show stype++" body"

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
        let mCd = getContentDisposition (atch^.partHeaders)
        ct `shouldBe` contentType a
        cst `shouldBe` contentSubtype a
        case contentDisposition a of
          Just cd -> fmap (^?cdType) mCd `shouldBe` Just (Just cd)
          _       -> fmap (^?cdType) mCd `shouldBe` Nothing
        forM_ (fileMatches a) $ checkMatch (atch^.partBody)
          


checkMatch :: Body -> Match -> Expectation
checkMatch b m
  = case (b, m) of
      (TextBody t, Match s)        -> t `shouldBe` s
      (TextBody t, Count n s)      -> T.count s t `shouldBe` n
      (BinaryBody t, BinMatch s)   -> t `shouldBe` s
      (BinaryBody t, BinCount n s) -> binCount s t `shouldBe` n
      _                            -> expectationFailure $
                                        "incompatible match: " ++ show (b, m)
            
binCount :: ByteString -> ByteString -> Int
binCount a b = go b 0
  where go c !n = case S.breakSubstring a c of
                    (_,c') | S.null c' -> n
                    (_,c')             -> go (S.tail c') (n+1)

textBody :: ByteString -> Message -> Maybe Body
textBody subtype m
  = case m^.msgBody of
      MultipartBody _ (x:_) _ ->
        case (getContentType (x^.partHeaders), x^.partBody) of
          (ContentType "multipart" "alternative" _, MultipartBody _ ps _) ->
            fmap (\p -> p^.partBody) (find isTextPart ps)
          (ContentType "text" st _, t) | st==subtype -> Just t
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
  = Match    Text
  | BinMatch ByteString
  | Count    Int Text
  | BinCount Int ByteString
  deriving (Eq, Show)

data AttachmentMatch
  = AttachmentMatch {
        filename           :: ByteString
      , size               :: Integer
      , fileMatches        :: [Match]
      , contentType        :: ByteString
      , contentSubtype     :: ByteString
      , contentDisposition :: Maybe ContentDispositionType
  } deriving (Eq, Show)
