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
import Control.Lens
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString, readFile)
import qualified Data.ByteString.Char8 as S
import Network.Mail.Mime.Parser
import Network.Mail.Mime.Parser.Internal.Common
import Prelude hiding (readFile)

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
              filename           = Just "attach01"
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
              filename           = Just "attach02"
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
              filename           = Just "attach03"
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
              filename           = Just "attach04"
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
              filename           = Just "attach05"
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
              filename           = Just "attach06"
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
              filename           = Just "attach02"
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
      , htmlMatches = [
          Count 1 "<center><h1>Testing MIME E-mail composing with cid</h1></center>",
          Count 1 "<table background=\"cid:4c837ed463ad29c820668e835a270e8a.jpg\" width=\"100%\">"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = Just "logo.jpg"
            , size           = 2695
            , fileMatches    = []
            , contentType    = "image"
            , contentSubtype = "gif"
            , contentDisposition = Just Inline
            },
          AttachmentMatch {
              filename       = Just "background.jpg"
            , size           = 18255
            , fileMatches    = []
            , contentType    = "image"
            , contentSubtype = "gif"
            , contentDisposition = Just Inline
            },
          AttachmentMatch {
              filename       = Just "attachment.txt"
            , size           = 2229
            , fileMatches    = [Count 4 "Sed pulvinar"]
            , contentType    = "text"
            , contentSubtype = "plain"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0009"
      , expectedHeaders = [
            From [NameAddr (Just "Ogone") "noreply@ogone.com"]
          , To [(NameAddr Nothing "info@testsite.com")]
          , Subject "Ogone NIEUWE order Maurits PAYID: 951597484 / orderID: 456123 / status: 5"
          ]
      , textMatches = [Count 1 "951597484"]
      }
  , fixture {
        mailId          = "m0010"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr Nothing "name@company2.com")]
          , Subject "Mail de 800ko without filename"
          ]
      , textMatches = [Match "\n"]
      , htmlMatches = [Count 1 "<div dir=\"ltr\"><br></div>"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = Nothing
            , size           = 817938
            , fileMatches    = [BinCount 726 "Suspendisse"]
            , contentType    = "application"
            , contentSubtype = "octet-stream"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0011"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr (Just "Name") "name@company.com")]
          , Subject "Hello World !"
          ]
      , textMatches = [Count 1 "This is a text body"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = Just "file.txt"
            , size           = 29
            , fileMatches    = [Count 1 "This is a file"]
            , contentType    = "text"
            , contentSubtype = "plain"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0012"
      , expectedHeaders = [
            From [NameAddr (Just "Name") "name@company.com"]
          , To [(NameAddr (Just "Name") "name@company.com")]
          , Subject "Hello World !"
          ]
      , textMatches = [Count 1 "This is a text body"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = Just "file.txt"
            , size           = 29
            , fileMatches    = [Count 1 "This is a file"]
            , contentType    = "text"
            , contentSubtype = "plain"
            , contentDisposition = Just Attachment
            }
          ]
      }
  , fixture {
        mailId          = "m0013"
      , expectedHeaders = [
            From [NameAddr (Just "NAME Firstname")
                           "firstname.name@groupe-company.com"]
          , To [(NameAddr (Just "paul.dupont@company.com")
                          "paul.dupont@company.com")]
          , Subject "50032266 CAR 11_MNPA00A01_9PTX_H00 ATT N° 1467829. pdf"
          ]
      , textMatches = [Count 1 "Superviseur de voitures"]
      , attachmentMatches = [
          AttachmentMatch {
              filename       = Just "50032266 CAR 11_MNPA00A01_9PTX_H00 ATT N° 1467829.pdf"
            , size           = 10
            , fileMatches    = []
            , contentType    = "application"
            , contentSubtype = "pdf"
            , contentDisposition = Just Attachment
            }
          ]
      }
  ]

parseIncrementally :: Parser a -> ByteString -> Either String a
parseIncrementally p bs = go ls r (0 :: Int)
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
          case getTextBody stype parsed of
            Just b -> checkMatch b m
            _      -> expectationFailure $ "email has no "++ show stype++" body"

  -- Check for expected headers
  forM_ expectedHeaders $ \h -> 
    it ("has expected header " ++ show h) $ tryParse $ \parsed ->
      if h `elem` parsed^.msgHeaders
        then return ()
        else expectationFailure $ concat ["Missing header: ", show h, " in ",
                                          show (parsed^.msgHeaders)]
  
  -- Check for Matches on text body
  forM_ textMatches $ textMatchSpec "plain"
 
  -- Check for Matches on html body
  forM_ htmlMatches $ textMatchSpec "html"

  -- Check for Matches on html body
  forM_ (zip [0..] attachmentMatches) $ \(i,a) ->
    it ("attachment " ++ show i ++ " matches " ++ show a) $
      tryParse $ \parsed -> do
        let atch = getAttachments parsed !! i
            size' = case atch^.partBody of
                      TextBody   t -> Just . fromIntegral $ T.length t
                      BinaryBody s -> Just . fromIntegral $ S.length s
                      _            -> Nothing 
        getFilename (atch^.partHeaders) `shouldBe` filename a
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
      (TextBody t, Count n s) ->
        let n' = T.count s t
        in if n' == n
           then return ()
           else expectationFailure $ concat [
              show n', "/=", show n, ":", show t]
      (BinaryBody t, BinMatch s)   -> t `shouldBe` s
      (BinaryBody t, BinCount n s) -> binCount s t `shouldBe` n
      _                            -> expectationFailure $
                                        "incompatible match: " ++ show (b, m)
            
binCount :: ByteString -> ByteString -> Int
binCount a b = go b 0
  where go c !n = case S.breakSubstring a c of
                    (_,c') | S.null c' -> n
                    (_,c')             -> go (S.tail c') (n+1)

                    
            

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
        filename           :: Maybe Text
      , size               :: Integer
      , fileMatches        :: [Match]
      , contentType        :: ByteString
      , contentSubtype     :: ByteString
      , contentDisposition :: Maybe ContentDispositionType
  } deriving (Eq, Show)
