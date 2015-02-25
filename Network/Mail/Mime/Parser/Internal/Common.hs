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
  , sToLower
  , getContentType
  , getBoundary
  , getEncoding
  , module Data.Attoparsec.ByteString.Char8
) where

import Control.Applicative ((*>), (<*), (<|>))
import Control.Lens hiding (noneOf)
import Data.Attoparsec.ByteString.Char8 hiding (isHorizontalSpace)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char (toLower)
import Data.Maybe (catMaybes)
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



--
-- Utils
--
sToLower :: ByteString -> ByteString
sToLower = S.map toLower

getBoundary :: [Field] -> Maybe ByteString
getBoundary = firstJust . concat . map (map (^?_Boundary) . (^.ctParms))

getContentType :: [Field] -> Maybe Field
getContentType = firstJust . map (fmap (review _ContentType) . (^?_ContentType))

getEncoding :: [Field] -> Maybe Encoding
getEncoding = firstJust . map (^?_ContentTransferEncoding)

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

firstJust :: [Maybe a] -> Maybe a
firstJust = headMay . catMaybes
