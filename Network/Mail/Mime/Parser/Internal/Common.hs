{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Common
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides a compatibility layer with Parsec and some utilities
 -}
module Network.Mail.Mime.Parser.Internal.Common (
    noneOf
  , between
  , trim
  , oneOf
  , isHorizontalSpace
  , isEndOfLine
  , readIntN
  , named
  , quoted
  , hex_octet
  , sToLower
  , getContentType
  , getBoundary
  , getCharset
  , getEncoding
  , getAttachments
  , getTextBody
  , getFilename
  , getContentDisposition
  , firstJust
  , ensureCRLFeols
  , module Data.Attoparsec.ByteString.Char8
) where

import Control.Applicative ((*>), (<*), (<|>))
import Control.Lens hiding (noneOf)
import Data.Attoparsec.ByteString.Char8 hiding (isHorizontalSpace, isEndOfLine)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as S
import Data.Char (toLower, chr)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Prelude hiding (take)
import Network.Mail.Mime.Parser.Types

-- |This parser will match a N digit number and return its integer value
readIntN :: Int -> Parser Int
readIntN n = do y <- take n
                case S.readInt y of
                  Just (r,"") -> return r
                  _           -> fail "readIntN"

noneOf :: String -> Parser Char
noneOf = satisfy . notInClass

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b p = a *> p <* b

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c==' ' || c=='\t'

isEndOfLine :: Char -> Bool
isEndOfLine c = c=='\r' || c=='\n'

trim :: Parser a -> Parser a
trim = between skipSpace skipSpace

named :: String -> Parser a -> Parser a
named = flip (<?>)

quoted :: Parser a -> Parser a
quoted = between "\"" "\""

hex_octet :: Parser ByteString
hex_octet
    = "="
 *> (take 2>>= either fail (return . S.singleton . chr) . parseOnly hexadecimal)


--
-- Utils
--
sToLower :: ByteString -> ByteString
sToLower = S.map toLower

getBoundary :: [ContentTypeParm] -> Maybe ByteString
getBoundary = firstJust . map (^?_Boundary)

getCharset :: [ContentTypeParm] -> ByteString
getCharset = fromMaybe defaultCharset . firstJust . map (^?_Charset)

getContentType :: [Field] -> Field
getContentType = fromMaybe defaultContentType
               . firstJust . map (fmap (review _ContentType) . (^?_ContentType))

getContentDisposition :: [Field] -> Maybe Field
getContentDisposition
  = firstJust
  . map (fmap (review _ContentDisposition) . (^?_ContentDisposition))

getEncoding :: [Field] -> Encoding
getEncoding = fromMaybe defaultEncoding
            . firstJust . map (^?_ContentTransferEncoding)

getAttachments :: Message -> [Part]
getAttachments = goBody . (^.msgBody)
  where
    goBody body
      = case body of
          MultipartBody _ ps _ -> concat $ map goPart ps
          _ -> []
    goPart p
      = case getContentType (p^.partHeaders) of
          ContentType "multipart" "alternative" _ -> []
          ContentType "multipart" "mixed" _ -> goBody (p^.partBody)
          ContentType "multipart" _       _ -> goBody (p^.partBody)
          _ | isAttachment (p^.partHeaders) -> [p]
          _ -> []
                  
isAttachment :: [Field] -> Bool
isAttachment hs = dispositionMatches || hasName
  where
    dispositionMatches
      = case getContentDisposition hs of
          Just (ContentDisposition Attachment _) -> True
          Just (ContentDisposition Inline     _) -> True
          _                                      -> False
    hasName = isJust (getFilename hs)

isInline :: [Field] -> Bool
isInline hs
  = case getContentDisposition hs of
          Just (ContentDisposition Inline     _) -> True
          _                                      -> False

getTextBody :: ByteString -> Message -> Maybe Body
getTextBody subtype m = go (m^.msgHeaders) (m^.msgBody)
  where
    go fs b
      = case b of
          MultipartBody _ ps _ ->
            firstJust . map (\p -> go (p^.partHeaders) (p^.partBody)) $ ps
          TextBody{} -> 
            case getContentType fs of
              ContentType "text" st _
                | st==subtype && (isInline fs || not (isAttachment fs)) ->
                  Just b
              _ -> Nothing
          _ -> Nothing

getFilename :: [Field] -> Maybe Text
getFilename fs
    = (firstJust . map (^?_Filename) . concat . map (^.cdParms) $ fs)
  <|> (firstJust . map (^?_Name) . concat . map (^.ctParms) $ fs)
 
defaultEncoding :: Encoding
defaultEncoding = Binary7Bit

defaultCharset :: ByteString
defaultCharset = "US-ASCII"

defaultContentType :: Field
defaultContentType = ContentType "text" "plain" [Charset defaultCharset]

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

firstJust :: [Maybe a] -> Maybe a
firstJust = headMay . catMaybes

ensureCRLFeols :: ByteString -> ByteString
ensureCRLFeols = S.intercalate "\r\n" . map go  . S.split '\n'
  where
    go s = case S.unsnoc s of
            Just (s', '\r') -> s'
            _               -> s
