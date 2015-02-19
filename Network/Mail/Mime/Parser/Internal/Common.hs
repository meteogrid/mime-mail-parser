{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Common
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides a compatibility layer with attoparsec
 -}
module Network.Mail.Mime.Parser.Internal.Common (
    noneOf
  , between
  , optional
  , oneOf
  , isHorizontalSpace
  , readIntN
  , module Data.Attoparsec.ByteString.Char8
) where

import Data.Attoparsec.ByteString.Char8 hiding (isHorizontalSpace)
import Control.Applicative ((*>), (<*), (<|>))
import Data.ByteString.Char8 (readInt)
import Prelude hiding (take)

-- |This parser will match a N digit number and return its integer value
readIntN :: Int -> Parser Int
readIntN n = do y <- take n
                case readInt y of
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
