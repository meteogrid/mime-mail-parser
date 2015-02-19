{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Text.ParserCombinators.Attoparsec.ParsecCompat
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides a compatibility layer with attoparsec
 -}
module Text.ParserCombinators.Attoparsec.ParsecCompat (
    noneOf
  , between
  , optional
  , oneOf
  , isHorizontalSpace
  , module Data.Attoparsec.ByteString.Char8
) where

import Data.Attoparsec.ByteString.Char8 hiding (isHorizontalSpace)
import Control.Applicative ((*>), (<*), (<|>))

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
