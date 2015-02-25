{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Unicode
   Copyright   :  (c) 2015 Alberto Valverde González
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides text decoding
-}

module Network.Mail.Mime.Parser.Internal.Unicode (toUnicode) where

import Control.Exception (try)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.ICU.Error as ICU
import System.IO.Unsafe (unsafePerformIO)

toUnicode :: String -> ByteString -> Either String Text
toUnicode encoding str = unsafePerformIO $ do
  eConverter <- tryICU $ ICU.open encoding (Just True)
  return $ case eConverter of
    Left _           -> Left $ "Invalid encoding: " ++ encoding
    Right converter  -> Right $ ICU.toUnicode converter str

tryICU :: IO a -> IO (Either ICU.ICUError a)
tryICU = try
