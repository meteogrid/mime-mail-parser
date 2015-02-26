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
  , optional
  , oneOf
  , isHorizontalSpace
  , readIntN
  , named
  , sToLower
  , getContentType
  , getBoundary
  , getCharset
  , getEncoding
  , getAttachments
  , getFilename
  , getContentDisposition
  , module Data.Attoparsec.ByteString.Char8
) where

import Control.Applicative ((*>), (<*), (<|>))
import Control.Lens hiding (noneOf)
import Data.Attoparsec.ByteString.Char8 hiding (isHorizontalSpace)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char (toLower)
import Data.Maybe (catMaybes, fromMaybe)
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

optional :: Parser a -> Parser ()
optional p = (p >> return ()) <|> return ()

oneOf :: String -> Parser Char
oneOf = satisfy . inClass

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c==' ' || c=='\t'

trim :: Parser a -> Parser a
trim = between skipSpace skipSpace

named :: String -> Parser a -> Parser a
named = flip (<?>)


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

getAttachments :: Body -> [Part]
getAttachments body
  = case body of
      MultipartBody _ ps _ -> concat $ map go ps
      _ -> []
  where
    go p = case getContentType (p^.partHeaders) of
             ContentType "multipart" "mixed" _ -> getAttachments (p^.partBody)
             ContentType "multipart" "alternative" _ -> []
             _ -> [p]
                  

getFilename :: [Field] -> Maybe ByteString
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
