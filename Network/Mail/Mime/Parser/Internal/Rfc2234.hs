{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2234
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2234, \"Augmented BNF for Syntax Specifications:
   ABNF\", <http://www.faqs.org/rfcs/rfc2234.html>. The
   terminal called @char@ in the RFC is called 'character'
   here to avoid conflicts with Attoparsec's 'char' function.
 -}

module Network.Mail.Mime.Parser.Internal.Rfc2234 where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char (chr, ord)
import Data.Monoid ((<>))
import Control.Applicative (many, (<|>))
import Control.Monad ( liftM2 )
import Network.Mail.Mime.Parser.Internal.Common

----------------------------------------------------------------------
-- * Primitive Parsers
----------------------------------------------------------------------

-- |Match any character of the alphabet.

alpha           :: Parser Char
alpha            = satisfy isAsciiAlpha
                   <?> "alphabetic character"

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = c `elem` (['A'..'Z'] ++ ['a'..'z'])

-- |Match either \"1\" or \"0\".

bit             :: Parser Char
bit              = oneOf "01"   <?> "bit ('0' or '1')"

-- |Match any 7-bit US-ASCII character except for NUL (ASCII value 0, that is).

character       :: Parser Char
character        = satisfy (\c -> (c >= chr 1) && (c <= chr 127))
                   <?> "7-bit character excluding NUL"

-- |Match the carriage return character @\\r@.

cr              :: Parser Char
cr               = char '\r'    <?> "carriage return"

-- |Match returns the linefeed character @\\n@.

lf              :: Parser Char
lf               = char '\n'    <?> "linefeed"

-- |Match the Internet newline @\\r\\n@.

crlf            :: Parser ByteString
crlf             = "\r\n" <?> "carriage return followed by linefeed"

-- |Match any US-ASCII control character. That is
-- any character with a decimal value in the range of [0..31,127].

ctl             :: Parser Char
ctl              = satisfy (\c -> ord c `elem` ([0..31] ++ [127]))
                   <?> "control character"

-- |Match the double quote character \"@\"@\".

dquote          :: Parser Char
dquote           = char (chr 34)    <?> "double quote"

-- |Match the tab (\"@\\t@\") character.

htab            :: Parser Char
htab             = char '\t'    <?> "horizontal tab"

-- |Match \"linear white-space\". That is any number of consecutive
-- 'wsp', optionally followed by a 'crlf' and (at least) one more
-- 'wsp'.

lwsp            :: Parser ByteString
lwsp             = do r <- choice
                           [ takeWhile1 isHorizontalSpace
                           , try (liftM2 (<>) crlf (takeWhile1 isHorizontalSpace))
                           ]
                      rs <- option "" lwsp
                      return (r <> rs)
                   <?> "linear white-space"

-- |Match /any/ character.
octet           :: Parser Char
octet            = anyChar    <?> "any 8-bit character"

-- |Match the space.

sp              :: Parser Char
sp               = char ' '    <?> "space"

-- |Match any printable ASCII character. (The \"v\" stands for
-- \"visible\".) That is any character in the decimal range of
-- [33..126].

vchar           :: Parser Char
vchar            = satisfy (\c -> (c >= chr 33) && (c <= chr 126))
                   <?> "printable character"

-- |Match either 'sp' or 'htab'.

wsp             :: Parser Char
wsp              = sp <|> htab    <?> "white-space"


-- ** Useful additions

-- |Match a \"quoted pair\". Any characters (excluding CR and
-- LF) may be quoted.

quoted_pair     :: Parser ByteString
quoted_pair      = do _ <- char '\\'
                      r <- noneOf "\r\n"
                      return $ S.pack ['\\',r]
                   <?> "quoted pair"

-- |Match a quoted string. The specials \"@\\@\" and
-- \"@\"@\" must be escaped inside a quoted string; CR and
-- LF are not allowed at all.

quoted_string   :: Parser ByteString
quoted_string    = do _ <- dquote
                      r <- many qcont
                      _ <- dquote
                      return ("\"" <> S.concat r <> "\"")
                   <?> "quoted string"
  where
  qcont = takeWhile1 (`notElem` "\\\"\r\n") <|> quoted_pair
