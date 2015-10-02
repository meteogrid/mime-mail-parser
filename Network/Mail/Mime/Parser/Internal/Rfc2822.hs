{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2822
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2822, \"Internet Message Format\",
   <http://www.faqs.org/rfcs/rfc2822.html>.
-}

module Network.Mail.Mime.Parser.Internal.Rfc2822 where

import System.Time
import Data.Char (ord)
import Data.Maybe (catMaybes)
import Control.Applicative (many, pure, optional, (<$>), (<*), (*>), (<|>))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Network.Mail.Mime.Parser.Internal.Rfc2234 hiding (quoted_pair, quoted_string)
import Network.Mail.Mime.Parser.Internal.Rfc2047 (encoded_word)
import Network.Mail.Mime.Parser.Types (
  Message(..), Body(..), Field(..), NameAddr(..))
import Network.Mail.Mime.Parser.Internal.Common
import Prelude hiding (takeWhile)

-- * Useful parser combinators

-- |Return @Nothing@ if the given parser doesn't match. This
-- combinator is included in the latest parsec distribution as
-- @optionMaybe@, but ghc-6.6.1 apparently doesn't have it.

maybeOption    :: Parser a -> Parser (Maybe a)
maybeOption p   = option Nothing (fmap Just p)

-- |@unfold@ @=@ @between (optional cfws) (optional cfws)@

unfold          :: Parser b -> Parser b
unfold           = between (optional cfws) (optional cfws)

-- |Construct a parser for a message header line from the
-- header's name and a parser for the body.

header          :: ByteString -> Parser b -> Parser b
header n p       = let nameString = stringCI (n <> ":") <* space
                   in
                   between nameString crlf p <?> (S.unpack n ++ " header line")

-- |Like 'header', but allows the obsolete white-space rules.

obs_header      :: ByteString -> Parser b -> Parser b
obs_header n p   = let nameString = stringCI n >> many wsp >> char ':'
                   in between nameString crlf p <?>
                       ("obsolete " ++ S.unpack n ++ " header line")


-- ** Primitive Tokens (section 3.2.1)

-- |Match any US-ASCII non-whitespace control character.

no_ws_ctl       :: Parser Char
no_ws_ctl       = satisfy isNoWsCtl
                  <?> "US-ASCII non-whitespace control character"

isNoWsCtl :: Char -> Bool
isNoWsCtl c = ord c `elem` ([1..8] ++ [11,12] ++ [14..31] ++ [127])

-- |Match any US-ASCII character except for @\r@, @\n@.

text            :: Parser Char
text            = satisfy isText
                  <?> "US-ASCII character (excluding CR and LF)"

isText :: Char -> Bool
isText c = ord c `elem` ([1..9] ++ [11,12] ++ [14..127])

-- |Match any of the RFC's \"special\" characters: @()\<\>[]:;\@,.\\\"@.

specials        :: Parser Char
specials        = oneOf "()<>[]:;@,.\\\""   <?> "one of ()<>[]:;@,.\\\""


-- ** Quoted characters (section 3.2.2)

-- |Match a \"quoted pair\". All characters matched by 'text' may be
-- quoted. Note that the parsers returns /both/ characters, the
-- backslash and the actual content.

quoted_pair     :: Parser ByteString
quoted_pair     = obs_qp <|> do {_ <- char '\\';
                                 r <- text;
                                 return (S.pack ['\\',r])}
                  <?> "quoted pair"


-- ** Folding white space and comments (section 3.2.3)

-- |Match \"folding whitespace\". That is any combination of 'wsp' and
-- 'crlf' followed by 'wsp'.

fws :: Parser ByteString
fws = S.concat <$> many1 (choice [blanks, linebreak])
  where
    blanks      = takeWhile1 isHorizontalSpace
    linebreak   = crlf *> blanks

-- |Match any non-whitespace, non-control character except for \"@(@\",
-- \"@)@\", and \"@\\@\". This is used to describe the legal content of
-- 'comment's.
--
-- /Note/: This parser accepts 8-bit characters, even though this is
-- not legal according to the RFC. Unfortunately, 8-bit content in
-- comments has become fairly common in the real world, so we'll just
-- accept the fact.

ctext           :: Parser Char
ctext           = satisfy isCtext
                  <?> "any regular character (excluding '(', ')', and '\\')"
isCtext :: Char -> Bool
isCtext c = isNoWsCtl c
         || ord c `elem` ([33..39] ++ [42..91] ++ [93..126] ++ [128..255])

-- |Match a \"comments\". That is any combination of 'ctext',
-- 'quoted_pair's, and 'fws' between brackets. Comments may nest.

comment         :: Parser ByteString
comment         = do _ <- char '('
                     r1 <- many ccontent
                     r2 <- option "" fws
                     _ <- char ')'
                     return ("(" <> S.concat r1 <> r2 <> ")")
                  <?> "comment"
    where
    ccontent    = try $ do r1 <- option "" fws
                           r2 <- choice [takeWhile1 isCtext, quoted_pair, comment]
                           return (r1 <> r2)

-- |Match any combination of 'fws' and 'comments'.

cfws            :: Parser ByteString
cfws            = do r <- many1 $ choice [ fws, comment ]
                     return (S.concat r)

-- ** Atom (section 3.2.4)

-- |Match any US-ASCII character except for control characters,
-- 'specials', or space. 'atom' and 'dot_atom' are made up of this.

atext           :: Parser Char
atext           = alpha <|> digit <|> oneOf "!#$%&'*+-/=?^_`{|}~"
                  <?> "US-ASCII character (excluding controls, space, and specials)"

atextstring :: Parser ByteString
atextstring = takeWhile1 (\c -> isAsciiAlpha c 
                             || isDigit c
                             || c `elem` ("!#$%&'*+-/=?^_`{|}~"::String))

-- |Match one or more 'atext' characters and skip any preceeding or
-- trailing 'cfws'.

atom            :: Parser ByteString
atom            = unfold (atextstring <?> "atom")

-- |Match 'dot_atom_text' and skip any preceeding or trailing 'cfws'.

dot_atom        :: Parser ByteString
dot_atom        = unfold (dot_atom_text <?> "dot atom")

-- |Match two or more 'atext's interspersed by dots.

dot_atom_text   :: Parser ByteString
dot_atom_text   = fmap (S.intercalate ".") (sepBy1 atextstring (char '.'))
                  <?> "dot atom content"


-- ** Quoted strings (section 3.2.5)

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

qtext           :: Parser Char
qtext           = satisfy isQText
                  <?> "US-ASCII character (excluding '\\', and '\"')"

isQText :: Char -> Bool
isQText c = isNoWsCtl c || ord c `elem` ([33] ++ [35..91] ++ [93..126])

-- |Match either 'qtext' or 'quoted_pair'.

qcontent        :: Parser ByteString
qcontent        = takeWhile1 isQText <|> quoted_pair
                  <?> "quoted string content"

-- |Match any number of 'qcontent' between double quotes. Any 'cfws'
-- preceeding or following the \"atom\" is skipped automatically.

quoted_string   :: Parser ByteString
quoted_string   = unfold quoted_string_text

quoted_string_no_quotes   :: Parser ByteString
quoted_string_no_quotes   = unfold quoted_string_text_no_quotes

quoted_string_text   :: Parser ByteString
quoted_string_text = (fmap (\s -> "\"" <> s <> "\"") 
                       quoted_string_text_no_quotes) <?> "quoted_string"

quoted_string_text_no_quotes :: Parser ByteString
quoted_string_text_no_quotes = quoted $ do
  r1 <- many (do r1 <- option "" fws
                 r2 <- qcontent
                 return (r1 <> r2))
  r2 <- option "" fws
  return (S.concat r1 <> r2)

-- * Miscellaneous tokens (section 3.2.6)

-- |Match either 'atom' or 'quoted_string'.

word            :: Parser ByteString
word            = unfold (atom <|> quoted_string)     <?> "word"

word_no_quotes :: Parser ByteString
word_no_quotes = unfold (atom <|> quoted_string_no_quotes)
             <?> "word_no_quotes"

-- |Match either one or more 'word's or an 'obs_phrase'.

phrase          :: Parser [Text]
phrase          = {- many1 word <?> "phrase" <|> -} obs_phrase

-- |Match any non-whitespace, non-control US-ASCII character except
-- for \"@\\@\" and \"@\"@\".

utext :: Parser ByteString
utext = fmap S.singleton no_ws_ctl
    <|> fmap S.singleton (satisfy (\c -> ord c `elem` [33..126]))
    <|> obs_utext
    <?> "regular US-ASCII character (excluding '\\', and '\"')"

-- |Match any number of 'utext' tokens.
--
-- \"Unstructured text\" is used in free text fields such as 'subject'.
-- Please note that any comments or whitespace that prefaces or
-- follows the actual 'utext' is /included/ in the returned string.

unstructured :: Parser Text
unstructured
  = do r1 <- fmap decodeUtf8 (option "" fws)
       r2 <- many (encoded <|> unencoded)
       return (r1 <> T.concat r2)
    <?> "unstructured text"
  where
    encoded = encoded_word <* optional fws
    unencoded = do
      r1 <- fmap decodeUtf8 utext
      r2 <- fmap decodeUtf8 (option "" fws)
      return (r1 <> r2)


-- * Date and Time Specification (section 3.3)

-- |Parse a date and time specification of the form
--
-- >   Thu, 19 Dec 2002 20:35:46 +0200
--
-- where the weekday specification \"@Thu,@\" is optional. The parser
-- returns a 'CalendarTime', which is set to the appropriate values.
-- Note, though, that not all fields of 'CalendarTime' will
-- necessarily be set correctly! Obviously, when no weekday has been
-- provided, the parser will set this field to 'Monday' - regardless
-- of whether the day actually is a monday or not. Similarly, the day
-- of the year will always be returned as @0@. The timezone name will
-- always be empty: @\"\"@.
--
-- Nor will the 'date_time' parser perform /any/ consistency checking.
-- It will accept
--
-- >    40 Apr 2002 13:12 +0100
--
-- as a perfectly valid date.
--
-- In order to get all fields set to meaningful values, and in order
-- to verify the date's consistency, you will have to feed it into any
-- of the conversion routines provided in "System.Time", such as
-- 'toClockTime'. (When doing this, keep in mind that most functions
-- return /local time/. This will not necessarily be the time you're
-- expecting.)

date_time       :: Parser CalendarTime
date_time       = do wd <- option Monday (try (do wd <- day_of_week
                                                  _ <- char ','
                                                  return wd))
                     (y,m,d) <- date
                     _ <- fws
                     (td,z) <- time
                     _ <- optional cfws
                     return (CalendarTime y m d (tdHour td) (tdMin td) (tdSec td) 0 wd 0 "" z False)
                  <?> "date/time specification"

-- |This parser matches a 'day_name' or an 'obs_day_of_week' (optionally
-- wrapped in folding whitespace) and return its 'Day' value.

day_of_week     :: Parser Day
day_of_week     =     try (between (optional fws) (optional fws) day_name <?> "name of a day-of-the-week")
                  <|> obs_day_of_week

-- |This parser will the abbreviated weekday names (\"@Mon@\", \"@Tue@\", ...)
-- and return the appropriate 'Day' value.

day_name        :: Parser Day
day_name        =     (stringCI "Mon" *> pure Monday)
                  <|> (stringCI "Tue" *> pure Tuesday)
                  <|> (stringCI "Wed" *> pure Wednesday)
                  <|> (stringCI "Thu" *> pure Thursday)
                  <|> (stringCI "Fri" *> pure Friday)
                  <|> (stringCI "Sat" *> pure Saturday)
                  <|> (stringCI "Sun" *> pure Sunday)
                  <?> "name of a day-of-the-week"

-- |This parser will match a date of the form \"@dd:mm:yyyy@\" and return
-- a tripple of the form (Int,Month,Int) - corresponding to
-- (year,month,day).

date            :: Parser (Int,Month,Int)
date            = do d <- day
                     m <- month
                     y <- year
                     return (y,m,d)
                  <?> "date specification"

-- |This parser will match a four digit number and return its integer
-- value. No range checking is performed.

year            :: Parser Int
year            = readIntN 4 <?> "year"

-- |This parser will match a 'month_name', optionally wrapped in
-- folding whitespace, or an 'obs_month' and return its 'Month'
-- value.

month           :: Parser Month
month           =     try (between (optional fws) (optional fws) month_name <?> "month name")
                  <|> obs_month


-- |This parser will the abbreviated month names (\"@Jan@\", \"@Feb@\", ...)
-- and return the appropriate 'Month' value.

month_name      :: Parser Month
month_name      =     (stringCI "Jan" *> pure January)
                  <|> (stringCI "Feb" *> pure February)
                  <|> (stringCI "Mar" *> pure March)
                  <|> (stringCI "Apr" *> pure April)
                  <|> (stringCI "May" *> pure May)
                  <|> (stringCI "Jun" *> pure June)
                  <|> (stringCI "Jul" *> pure July)
                  <|> (stringCI "Aug" *> pure August)
                  <|> (stringCI "Sep" *> pure September)
                  <|> (stringCI "Oct" *> pure October)
                  <|> (stringCI "Nov" *> pure November)
                  <|> (stringCI "Dec" *> pure December)
                  <?> "month name"

-- Internal helper function: match a 1 or 2-digit number (day of month).

day_of_month    :: Parser Int
day_of_month    = readIntN 2 <|> readIntN 1

-- |Match a 1 or 2-digit number (day of month), recognizing both
-- standard and obsolete folding syntax.

day             :: Parser Int
day             = try obs_day <|> day_of_month <?> "day"

-- |This parser will match a 'time_of_day' specification followed by a
-- 'zone'. It returns the tuple (TimeDiff,Int) corresponding to the
-- return values of either parser.

time            :: Parser (TimeDiff,Int)
time            = do t <- time_of_day
                     _ <- fws
                     z <- zone
                     return (t,z)
                  <?> "time and zone specification"

-- |This parser will match a time-of-day specification of \"@hh:mm@\" or
-- \"@hh:mm:ss@\" and return the corrsponding time as a 'TimeDiff'.

time_of_day     :: Parser TimeDiff
time_of_day     = do h <- hour
                     _ <- char ':'
                     m <- minute
                     s <- option 0 (do { _ <- char ':'; second } )
                     return (TimeDiff 0 0 0 h m s 0)
                  <?> "time specification"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking is performed.

hour            :: Parser Int
hour            = readIntN 2 <?> "hour"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking is performed.

minute          :: Parser Int
minute          = readIntN 2 <?> "minute"

-- |This parser will match a two-digit number and return its integer
-- value. No range checking takes place.

second          :: Parser Int
second          = readIntN 2 <?> "second"

-- |This parser will match a timezone specification of the form
-- \"@+hhmm@\" or \"@-hhmm@\" and return the zone's offset to UTC in
-- seconds as an integer. 'obs_zone' is matched as well.

zone            :: Parser Int
zone            = (    do _ <- char '+'
                          h <- hour
                          m <- minute
                          return (((h*60)+m)*60)
                   <|> do _ <- char '-'
                          h <- hour
                          m <- minute
                          return (-((h*60)+m)*60)
                   <?> "time zone"
                  )
                  <|> obs_zone


-- * Address Specification (section 3.4)

-- |Parse a single 'mailbox' or an address 'group' and return the
-- address(es).

address         :: Parser [NameAddr]
address         = try (do { r <- mailbox; return [r] }) <|> group
                  <?> "address"

-- |Parse a 'name_addr' or an 'addr_spec' and return the
-- address.

mailbox         :: Parser NameAddr
mailbox         = try name_addr <|> fmap (NameAddr Nothing) addr_spec
                  <?> "mailbox"

-- |Parse an 'angle_addr', optionally prefaced with a 'display_name',
-- and return the address.

name_addr       :: Parser NameAddr
name_addr       = do name <- maybeOption display_name
                     addr <- angle_addr
                     return (NameAddr name addr)
                  <?> "name address"

-- |Parse an 'angle_addr' or an 'obs_angle_addr' and return the address.

angle_addr      :: Parser ByteString
angle_addr      = try (unfold (do _ <- char '<'
                                  r <- addr_spec
                                  _ <- char '>'
                                  return r)
                       <?> "angle address"
                      )
                  <|> obs_angle_addr

-- |Parse a \"group\" of addresses. That is a 'display_name', followed
-- by a colon, optionally followed by a 'mailbox_list', followed by a
-- semicolon. The found address(es) are returned - what may be none.

group           :: Parser [NameAddr]
group           = do _ <- display_name
                     _ <- char ':'
                     r <- option [] mailbox_list
                     _ <- unfold $ char ';'
                     return r
                  <?> "address group"

-- |Parse and return a 'phrase'.

display_name    :: Parser Text
display_name    = fmap T.unwords phrase
                  <?> "display name"

-- |Parse a list of 'mailbox' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

mailbox_list    :: Parser [NameAddr]
mailbox_list    = sepBy mailbox (char ',') <?> "mailbox list"

-- |Parse a list of 'address' addresses, every two addresses being
-- separated by a comma, and return the list of found address(es).

address_list    :: Parser [NameAddr]
address_list    = do { r <-sepBy address (char ','); return (concat r) }
                  <?> "address list"


-- ** Addr-spec specification (section 3.4.1)

-- |Parse an \"address specification\". That is a 'local_part', followed
-- by an \"@\@@\" character, followed by a 'domain'. Return the complete
-- address as 'ByteString', ignoring any whitespace or any comments.

addr_spec       :: Parser ByteString
addr_spec       = do r1 <- local_part
                     _ <- char '@'
                     r2 <- domain
                     return (r1 <> "@" <> r2)
                  <?> "address specification"

-- |Parse and return a \"local part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'quoted_string'.

local_part      :: Parser ByteString
local_part      = obs_local_part <|> dot_atom_text <|> quoted_string_text
                  <?> "address' local part"

-- |Parse and return a \"domain part\" of an 'addr_spec'. That is either
-- a 'dot_atom' or a 'domain_literal'.

domain          :: Parser ByteString
domain          = obs_domain <|> dot_atom_text <|> domain_literal_text
                  <?> "address' domain part"

-- |Parse a \"domain literal\". That is a \"@[@\" character, followed by
-- any amount of 'dcontent', followed by a terminating \"@]@\"
-- character. The complete string is returned verbatim.

domain_literal  :: Parser ByteString
domain_literal  = unfold domain_literal_text

domain_literal_text  :: Parser ByteString
domain_literal_text = (do
  _ <- char '['
  r <- many (optional fws >> dcontent)
  _ <- optional fws
  _ <- char ']'
  return ("[" <> S.concat r <> "]")) <?> "domain literal"

-- |Parse and return any characters that are legal in a
-- 'domain_literal'. That is 'dtext' or a 'quoted_pair'.

dcontent        :: Parser ByteString
dcontent        = takeWhile1 isDText <|> quoted_pair
                  <?> "domain literal content"

-- |Parse and return any ASCII characters except \"@[@\", \"@]@\", and
-- \"@\\@\".

dtext           :: Parser Char
dtext           = satisfy isDText

isDText :: Char -> Bool
isDText c = isNoWsCtl c || ord c `elem` ([33..90] ++ [94..126])

-- * Overall message syntax (section 3.5)


-- |Parse a complete message as defined by this RFC and it broken down
-- into the separate header fields and the message body. Header lines,
-- which contain syntax errors, will not cause the parser to abort.
-- Rather, these headers will appear as 'OptionalField's (which are
-- unparsed) in the resulting 'Message'. A message must be really,
-- really badly broken for this parser to fail.
--
-- This behaviour was chosen because it is impossible to predict what
-- the user of this module considers to be a fatal error;
-- traditionally, parsers are very forgiving when it comes to Internet
-- messages.
--
-- If you want to implement a really strict parser, you'll have to put
-- the appropriate parser together yourself. You'll find that this is
-- rather easy to do. Refer to the 'fields' parser for further details.

rfc2822_message :: Parser Message
rfc2822_message = do
  f <- rfc2822_fields
  b <- option "" (crlf *> body)
  return (Message f (BinaryBody b))

-- |A message body is just an unstructured sequence of characters.

body            :: Parser ByteString
body            = takeByteString


-- * Field definitions (section 3.6)


-- |This parser will parse an arbitrary number of header fields as
-- defined in this RFC. For each field, an appropriate 'Field' value
-- is created, all of them making up the 'Field' list that this parser
-- returns.
--
-- If you look at the implementation of this parser, you will find
-- that it uses Parsec's 'try' modifier around /all/ of the fields.
-- The idea behind this is that fields, which contain syntax errors,
-- fall back to the catch-all 'optional_field'. Thus, this parser will
-- hardly ever return a syntax error -- what conforms with the idea
-- that any message that can possibly be accepted /should/ be.

rfc2822_fields :: Parser [Field]
rfc2822_fields = many rfc2822_field

rfc2822_field :: Parser Field
rfc2822_field
    = From            <$> from
  <|> Sender          <$> sender
  <|> ReturnPath      <$> return_path
  <|> ReplyTo         <$> reply_to
  <|> To              <$> to
  <|> Cc              <$> cc
  <|> Bcc             <$> bcc
  <|> MessageID       <$> message_id
  <|> InReplyTo       <$> in_reply_to
  <|> References      <$> references
  <|> Subject         <$> subject
  <|> Comments        <$> comments
  <|> Keywords        <$> keywords
  <|> Date            <$> orig_date
  <|> ResentDate      <$> resent_date
  <|> ResentFrom      <$> resent_from
  <|> ResentSender    <$> resent_sender
  <|> ResentTo        <$> resent_to
  <|> ResentCc        <$> resent_cc
  <|> ResentBcc       <$> resent_bcc
  <|> ResentMessageID <$> resent_msg_id
  <|> Received        <$> received
  <|> uncurry Field   <$> optional_field

-- ** The origination date field (section 3.6.1)

-- |Parse a \"@Date:@\" header line and return the date it contains a
-- 'CalendarTime'.

orig_date       :: Parser CalendarTime
orig_date       = header "Date" date_time


-- ** Originator fields (section 3.6.2)

-- |Parse a \"@From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

from            :: Parser [NameAddr]
from            = header "From" mailbox_list

-- |Parse a \"@Sender:@\" header line and return the 'mailbox' address
-- contained in it.

sender          :: Parser NameAddr
sender          = header "Sender" mailbox

-- |Parse a \"@Reply-To:@\" header line and return the 'address_list'
-- address(es) contained in it.

reply_to        :: Parser [NameAddr]
reply_to        = header "Reply-To" address_list


-- ** Destination address fields (section 3.6.3)

-- |Parse a \"@To:@\" header line and return the 'address_list'
-- address(es) contained in it.

to              :: Parser [NameAddr]
to              = header "To" address_list

-- |Parse a \"@Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

cc              :: Parser [NameAddr]
cc              = header "Cc" address_list

-- |Parse a \"@Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it.

bcc             :: Parser [NameAddr]
bcc             = header "Bcc" (address_list <|> cfws *> pure [])

-- ** Identification fields (section 3.6.4)

-- |Parse a \"@Message-Id:@\" header line and return the 'msg_id'
-- contained in it.

message_id      :: Parser ByteString
message_id      = header "Message-ID" msg_id

-- |Parse a \"@In-Reply-To:@\" header line and return the list of
-- 'msg_id's contained in it.

in_reply_to     :: Parser [ByteString]
in_reply_to     = header "In-Reply-To" (many1 msg_id)

-- |Parse a \"@References:@\" header line and return the list of
-- 'msg_id's contained in it.

references      :: Parser [ByteString]
references      = header "References" (many1 msg_id)

-- |Parse a \"@message ID:@\" and return it. A message ID is almost
-- identical to an 'angle_addr', but with stricter rules about folding
-- and whitespace.

msg_id          :: Parser ByteString
msg_id          = unfold (do _ <- char '<'
                             idl <- id_left
                             _ <- char '@'
                             idr <- id_right
                             _ <- char '>'
                             return ("<" <> idl <> "@" <> idr <> ">"))
                  <?> "message ID"

-- |Parse a \"left ID\" part of a 'msg_id'. This is almost identical to
-- the 'local_part' of an e-mail address, but with stricter rules
-- about folding and whitespace.

id_left         :: Parser ByteString
id_left         = dot_atom_text <|> no_fold_quote
                  <?> "left part of an message ID"

-- |Parse a \"right ID\" part of a 'msg_id'. This is almost identical to
-- the 'domain' of an e-mail address, but with stricter rules about
-- folding and whitespace.

id_right        :: Parser ByteString
id_right        = dot_atom_text <|> no_fold_literal
                  <?> "right part of an message ID"

-- |Parse one or more occurences of 'qtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_left' of a
-- 'msg_id'.

no_fold_quote   :: Parser ByteString
no_fold_quote   = do _ <- dquote
                     r <- many (takeWhile1 isQText <|> quoted_pair)
                     _ <- dquote
                     return ("\"" <> S.concat r <> "\"")
                  <?> "non-folding quoted string"

-- |Parse one or more occurences of 'dtext' or 'quoted_pair' and
-- return the concatenated string. This makes up the 'id_right' of a
-- 'msg_id'.

no_fold_literal :: Parser ByteString
no_fold_literal = do _ <- char '['
                     r <- many (takeWhile1 isDText <|> quoted_pair)
                     _ <- char ']'
                     return ("[" <> S.concat r <> "]")
                  <?> "non-folding domain literal"


-- ** Informational fields (section 3.6.5)

-- |Parse a \"@Subject:@\" header line and return its contents verbatim.
-- Please note that all whitespace and/or comments are preserved, i.e.
-- the result of parsing @\"Subject: foo\"@ is @\" foo\"@, not @\"foo\"@.

subject         :: Parser Text
subject         = header "Subject" unstructured

-- |Parse a \"@Comments:@\" header line and return its contents verbatim.
-- Please note that all whitespace and/or comments are preserved, i.e.
-- the result of parsing @\"Comments: foo\"@ is @\" foo\"@, not @\"foo\"@.

comments        :: Parser Text
comments        = header "Comments" unstructured

-- |Parse a \"@Keywords:@\" header line and return the list of 'phrase's
-- found. Please not that each phrase is again a list of 'atom's, as
-- returned by the 'phrase' parser.

keywords        :: Parser [[Text]]
keywords        = header "Keywords" (do r1 <- phrase
                                        r2 <- many (do _ <- char ','; phrase)
                                        return (r1:r2))


-- ** Resent fields (section 3.6.6)

-- |Parse a \"@Resent-Date:@\" header line and return the date it
-- contains as 'CalendarTime'.

resent_date     :: Parser CalendarTime
resent_date     = header "Resent-Date" date_time

-- |Parse a \"@Resent-From:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_from     :: Parser [NameAddr]
resent_from     = header "Resent-From" mailbox_list


-- |Parse a \"@Resent-Sender:@\" header line and return the 'mailbox_list'
-- address(es) contained in it.

resent_sender   :: Parser NameAddr
resent_sender   = header "Resent-Sender" mailbox


-- |Parse a \"@Resent-To:@\" header line and return the 'mailbox'
-- address contained in it.

resent_to       :: Parser [NameAddr]
resent_to       = header "Resent-To" address_list

-- |Parse a \"@Resent-Cc:@\" header line and return the 'address_list'
-- address(es) contained in it.

resent_cc       :: Parser [NameAddr]
resent_cc       = header "Resent-Cc" address_list

-- |Parse a \"@Resent-Bcc:@\" header line and return the 'address_list'
-- address(es) contained in it. (This list may be empty.)

resent_bcc      :: Parser [NameAddr]
resent_bcc      = header "Resent-Bcc" (address_list <|> (cfws *> pure []))
                  <?> "Resent-Bcc: header line"

-- |Parse a \"@Resent-Message-ID:@\" header line and return the 'msg_id'
-- contained in it.

resent_msg_id   :: Parser ByteString
resent_msg_id   = header "Resent-Message-ID" msg_id


-- ** Trace fields (section 3.6.7)

return_path     :: Parser ByteString
return_path     = header "Return-Path" path

path            :: Parser ByteString
path            = unfold ( try (do _ <- char '<'
                                   r <- option "" addr_spec
                                   _ <- char '>'
                                   return ("<" <> r <> ">")
                               )
                          <|> obs_path
                         )
                  <?> "return path spec"

received        :: Parser ([(ByteString,ByteString)], CalendarTime)
received        = header "Received" (do r1 <- name_val_list
                                        _ <- char ';'
                                        r2 <- date_time
                                        return (r1,r2))

name_val_list   :: Parser [(ByteString,ByteString)]
name_val_list   = optional cfws *> many1 name_val_pair
                  <?> "list of name/value pairs"

name_val_pair   :: Parser (ByteString,ByteString)
name_val_pair   = do r1 <- item_name
                     _ <- cfws
                     r2 <- item_value
                     return (r1,r2)
                  <?> "a name/value pair"

item_name       :: Parser ByteString
item_name       = do r1 <- alpha
                     r2 <- takeWhile
                             (\c -> c=='-' || isAsciiAlpha c || isDigit c)
                     return (r1 `S.cons` r2)
                  <?> "name of a name/value pair"

item_value      :: Parser ByteString
item_value      = choice [ try (do { r <- many1 angle_addr; return (S.concat r) })
                         , try addr_spec
                         , try domain
                         , msg_id
                         , try atom
                         ]
                  <?> "value of a name/value pair"

-- ** Optional fields (section 3.6.8)

-- |Parse an arbitrary header field and return a tuple containing the
-- 'field_name' and 'unstructured' text of the header. The name will
-- /not/ contain the terminating colon.

optional_field  :: Parser (ByteString,Text)
optional_field  = do n <- field_name
                     _ <- char ':'
                     skipSpace
                     b <- unstructured
                     _ <- crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"

-- |Parse and return an arbitrary header field name. That is one or
-- more 'ftext' characters.

field_name      :: Parser ByteString
field_name      = takeWhile1 isFText <?> "header line name"

-- |Match and return any ASCII character except for control
-- characters, whitespace, and \"@:@\".

ftext           :: Parser Char
ftext           = satisfy isFText
                  <?> "character (excluding controls, space, and ':')"

isFText :: Char -> Bool
isFText c = ord c `elem` ([33..57] ++ [59..126])

-- * Miscellaneous obsolete tokens (section 4.1)

-- |Match the obsolete \"quoted pair\" syntax, which - unlike
-- 'quoted_pair' - allowed /any/ ASCII character to be specified when
-- quoted. The parser will return both, the backslash and the actual
-- character.

obs_qp          :: Parser ByteString
obs_qp          = do _ <- char '\\'
                     c <- satisfy (\c -> ord c `elem` [0..127])
                     return (S.pack ['\\',c])
                  <?> "any quoted US-ASCII character"

-- |Match the obsolete \"text\" syntax, which - unlike 'text' - allowed
-- \"carriage returns\" and \"linefeeds\". This is really weird; you
-- better consult the RFC for details. The parser will return the
-- complete string, including those special characters.

obs_text        :: Parser ByteString
obs_text        = do let lfs = takeWhile (=='\n')
                         crs = takeWhile (=='\r')
                     r1 <- lfs
                     r2 <- crs
                     r3 <- many1 (do r4 <- obs_char
                                     r5 <- lfs
                                     r6 <- crs
                                     return (r4 `S.cons` (r5 <> r6)))
                     return (r1 <> r2 <> S.concat r3)

-- |Match and return the obsolete \"char\" syntax, which - unlike
-- 'character' - did not allow \"carriage return\" and \"linefeed\".

obs_char        :: Parser Char
obs_char        = satisfy (\c -> ord c `elem` ([0..9] ++ [11,12] ++ [14..127]))
                  <?> "any ASCII character except CR and LF"

-- |Match and return the obsolete \"utext\" syntax, which is identical
-- to 'obs_text'.

obs_utext       :: Parser ByteString
obs_utext       = obs_text

-- |Match the obsolete \"phrase\" syntax, which - unlike 'phrase' -
-- allows dots between tokens.

obs_phrase      :: Parser [Text]
obs_phrase      = do let wordT = fmap decodeUtf8 word_no_quotes
                     r1 <- encoded_word <|> wordT
                     r2 <- many $ choice [ encoded_word <|> wordT
                                         , string "." *> pure "."
                                         , cfws *> pure ""
                                         ]
                     return (r1 : filter (not . T.null) r2)

-- |Match a  \"phrase list\" syntax and return the list of 'String's
-- that make up the phrase. In contrast to a 'phrase', the
-- 'obs_phrase_list' separates the individual words by commas. This
-- syntax is - as you will have guessed - obsolete.

obs_phrase_list :: Parser [Text]
obs_phrase_list = do r1 <- many1 (do r <- option [] phrase
                                     _ <- unfold $ char ','
                                     return (filter (not . T.null) r))
                     r2 <- option [] phrase
                     return (concat r1 ++ r2)
                  <|> phrase


-- * Obsolete folding white space (section 4.2)

-- |Parse and return an \"obsolete fws\" token. That is at least one
-- 'wsp' character, followed by an arbitrary number (including zero)
-- of 'crlf' followed by at least one more 'wsp' character.

obs_fws         :: Parser ByteString
obs_fws         = do r1 <- takeWhile1 isHorizontalSpace
                     r2 <- many (do r3 <- crlf
                                    r4 <- takeWhile1 isHorizontalSpace
                                    return (r3 <> r4))
                     return (r1 <> S.concat r2)


-- * Obsolete Date and Time (section 4.3)

-- |Parse a 'day_name' but allow for the obsolete folding syntax.

obs_day_of_week :: Parser Day
obs_day_of_week = unfold day_name <?> "day-of-the-week name"

-- |Parse a 'year' but allow for a two-digit number (obsolete) and the
-- obsolete folding syntax.

obs_year        :: Parser Int
obs_year        = unfold (normalize <$> readIntN 2)
                  <?> "year"
    where
    normalize n
        | n <= 49   = 2000 + n
        | n <= 999  = 1900 + n
        | otherwise = n

-- |Parse a 'month_name' but allow for the obsolete folding syntax.

obs_month       :: Parser Month
obs_month       = between cfws cfws month_name <?> "month name"

-- |Parse a 'day' but allow for the obsolete folding syntax.

obs_day         :: Parser Int
obs_day         = unfold day_of_month <?> "day"

-- |Parse a 'hour' but allow for the obsolete folding syntax.

obs_hour        :: Parser Int
obs_hour        = unfold hour <?> "hour"

-- |Parse a 'minute' but allow for the obsolete folding syntax.

obs_minute      :: Parser Int
obs_minute      = unfold minute <?> "minute"

-- |Parse a 'second' but allow for the obsolete folding syntax.

obs_second      :: Parser Int
obs_second      = unfold second <?> "second"

-- |Match the obsolete zone names and return the appropriate offset.

obs_zone        :: Parser Int
obs_zone        = choice [ mkZone "UT"  0
                         , mkZone "GMT" 0
                         , mkZone "EST" (-5)
                         , mkZone "EDT" (-4)
                         , mkZone "CST" (-6)
                         , mkZone "CDT" (-5)
                         , mkZone "MST" (-7)
                         , mkZone "MDT" (-6)
                         , mkZone "PST" (-8)
                         , mkZone "PDT" (-7)
                         , do { r <- oneOf ['A'..'I']; return $ (ord r - 64) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['K'..'M']; return $ (ord r - 65) * 60*60 }  <?> "military zone spec"
                         , do { r <- oneOf ['N'..'Y']; return $ -(ord r - 77) * 60*60 } <?> "military zone spec"
                         , do { _ <- char 'Z'; return 0 }                               <?> "military zone spec"
                         ]
    where mkZone n o  = try $ do { _ <- string n; return (o*60*60) }


-- * Obsolete Addressing (section 4.4)

-- |This parser matches the \"obsolete angle address\" syntax, a construct that
-- used to be called \"route address\" in earlier RFCs. It differs from a
-- standard 'angle_addr' in two ways: (1) it allows far more
-- liberal insertion of folding whitespace and comments and (2) the address may
-- contain a \"route\" (which this parser ignores)

obs_angle_addr  :: Parser ByteString
obs_angle_addr  = unfold (do _ <- char '<'
                             _ <- option [] obs_route
                             addr <- addr_spec
                             _ <- char '>'
                             return ("<" <> addr <> ">") -- TODO: route is lost here.
                         )
                  <?> "obsolete angle address"

-- |This parser parses the \"route\" part of 'obs_angle_addr' and
-- returns the list of 'String's that make up this route. Relies on
-- 'obs_domain_list' for the actual parsing.

obs_route       :: Parser [ByteString]
obs_route       = unfold (do { r <- obs_domain_list; _ <- char ':'; return r })
                  <?> "route of an obsolete angle address"

-- |This parser parses a list of domain names, each of them prefaced
-- with an \"at\". Multiple names are separated by a comma. The list of
-- 'domain's is returned - and may be empty.

obs_domain_list :: Parser [ByteString]
obs_domain_list = do _ <- char '@'
                     r1 <- domain
                     r2 <- many (   (cfws <|> string ",")
                                 *> optional cfws
                                 *> "@"
                                 *> domain
                                )
                     return (r1 : r2)
                    <?> "route of an obsolete angle address"

-- |Parse the obsolete syntax of a 'local_part', which allowed for
-- more liberal insertion of folding whitespace and comments. The
-- actual string is returned.

obs_local_part  :: Parser ByteString
obs_local_part  = do r1 <- word
                     r2 <- many (do _ <- string "."
                                    r <- word
                                    return ('.' `S.cons` r))
                     return (r1 <> S.concat r2)
                  <?> "local part of an address"

-- |Parse the obsolete syntax of a 'domain', which allowed for more
-- liberal insertion of folding whitespace and comments. The actual
-- string is returned.

obs_domain      :: Parser ByteString
obs_domain      = do r1 <- atom
                     r2 <- many (do _ <- string "."
                                    r <- atom
                                    return ('.' `S.cons` r))
                     return (r1 <> S.concat r2)
                  <?> "domain part of an address"

-- |This parser will match the obsolete syntax for a 'mailbox_list'.
-- This one is quite weird: An 'obs_mbox_list' contains an arbitrary
-- number of 'mailbox'es - including none -, which are separated by
-- commas. But you may have multiple consecutive commas without giving
-- a 'mailbox'. You may also have a valid 'obs_mbox_list' that
-- contains /no/ 'mailbox' at all. On the other hand, you /must/ have
-- at least one comma.

obs_mbox_list   :: Parser [NameAddr]
obs_mbox_list   = do r1 <- many1 (try (do r <- maybeOption mailbox
                                          _ <- unfold $ char ','
                                          return r))
                     r2 <- maybeOption mailbox
                     return (catMaybes (r1 ++ [r2]))
                  <?> "obsolete syntax for a list of mailboxes"

-- |This parser is identical to 'obs_mbox_list' but parses a list of
-- 'address'es rather than 'mailbox'es. The main difference is that an
-- 'address' may contain 'group's. Please note that as of now, the
-- parser will return a simple list of addresses; the grouping
-- information is lost.

obs_addr_list   :: Parser [NameAddr]
obs_addr_list   = do r1 <- many1 (   maybeOption address
                                  <* optional cfws
                                  <* ","
                                  <* optional cfws
                                 )
                     r2 <- maybeOption address
                     return (concat (catMaybes (r1 ++ [r2])))
                  <?> "obsolete syntax for a list of addresses"


-- * Obsolete header fields (section 4.5)

obs_fields :: Parser [Field]
obs_fields
  = many
    (   From            <$> obs_from
    <|> Sender          <$> obs_sender
    <|> ReturnPath      <$> obs_return
    <|> ReplyTo         <$> obs_reply_to
    <|> To              <$> obs_to
    <|> Cc              <$> obs_cc
    <|> Bcc             <$> obs_bcc
    <|> MessageID       <$> obs_message_id
    <|> InReplyTo       <$> obs_in_reply_to
    <|> References      <$> obs_references
    <|> Subject         <$> obs_subject
    <|> Comments        <$> obs_comments
    <|> Keywords . (:[])<$> obs_keywords
    <|> Date            <$> obs_orig_date
    <|> ResentDate      <$> obs_resent_date
    <|> ResentFrom      <$> obs_resent_from
    <|> ResentSender    <$> obs_resent_send
    <|> ResentTo        <$> obs_resent_to
    <|> ResentCc        <$> obs_resent_cc
    <|> ResentBcc       <$> obs_resent_bcc
    <|> ResentMessageID <$> obs_resent_mid
    <|> ObsReceived     <$> obs_received
    <|> uncurry Field   <$> obs_optional
    )


-- ** Obsolete origination date field (section 4.5.1)

-- |Parse a 'date' header line but allow for the obsolete
-- folding syntax.

obs_orig_date   :: Parser CalendarTime
obs_orig_date   = obs_header "Date" date_time


-- ** Obsolete originator fields (section 4.5.2)

-- |Parse a 'from' header line but allow for the obsolete
-- folding syntax.

obs_from        :: Parser [NameAddr]
obs_from        = obs_header "From" mailbox_list

-- |Parse a 'sender' header line but allow for the obsolete
-- folding syntax.

obs_sender      :: Parser NameAddr
obs_sender      = obs_header "Sender" mailbox

-- |Parse a 'reply_to' header line but allow for the obsolete
-- folding syntax.

obs_reply_to    :: Parser [NameAddr]
obs_reply_to    = obs_header "Reply-To" mailbox_list


-- ** Obsolete destination address fields (section 4.5.3)

-- |Parse a 'to' header line but allow for the obsolete
-- folding syntax.

obs_to          :: Parser [NameAddr]
obs_to          = obs_header "To" address_list

-- |Parse a 'cc' header line but allow for the obsolete
-- folding syntax.

obs_cc          :: Parser [NameAddr]
obs_cc          = obs_header "Cc" address_list

-- |Parse a 'bcc' header line but allow for the obsolete
-- folding syntax.

obs_bcc         :: Parser [NameAddr]
obs_bcc         = header "Bcc" (address_list <|> (optional cfws *> pure []))


-- ** Obsolete identification fields (section 4.5.4)

-- |Parse a 'message_id' header line but allow for the obsolete
-- folding syntax.

obs_message_id  :: Parser ByteString
obs_message_id  = obs_header "Message-ID" msg_id

-- |Parse an 'in_reply_to' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_in_reply_to :: Parser [ByteString]
obs_in_reply_to
  = obs_header "In-Reply-To" (do r <- many (do {_ <- phrase; return "" }
                                            <|> msg_id)
                                 return (filter (not . S.null) r))

-- |Parse a 'references' header line but allow for the obsolete
-- folding and the obsolete phrase syntax.

obs_references  :: Parser [ByteString]
obs_references
  = obs_header "References" (do r <- many (do { _ <- phrase; return "" }
                                           <|> msg_id)
                                return (filter (not . S.null) r))

-- |Parses the \"left part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'local_part'.

obs_id_left     :: Parser ByteString
obs_id_left     = local_part <?> "left part of an message ID"

-- |Parses the \"right part\" of a message ID, but allows the obsolete
-- syntax, which is identical to a 'domain'.

obs_id_right    :: Parser ByteString
obs_id_right    = domain <?> "right part of an message ID"



-- ** Obsolete informational fields (section 4.5.5)

-- |Parse a 'subject' header line but allow for the obsolete
-- folding syntax.

obs_subject     :: Parser Text
obs_subject     = obs_header "Subject" unstructured

-- |Parse a 'comments' header line but allow for the obsolete
-- folding syntax.

obs_comments    :: Parser Text
obs_comments    = obs_header "Comments" unstructured

-- |Parse a 'keywords' header line but allow for the obsolete
-- folding syntax. Also, this parser accepts 'obs_phrase_list'.

obs_keywords    :: Parser [Text]
obs_keywords    = obs_header "Keywords" obs_phrase_list


-- ** Obsolete resent fields (section 4.5.6)

-- |Parse a 'resent_from' header line but allow for the obsolete
-- folding syntax.

obs_resent_from :: Parser [NameAddr]
obs_resent_from = obs_header "Resent-From" mailbox_list

-- |Parse a 'resent_sender' header line but allow for the obsolete
-- folding syntax.

obs_resent_send :: Parser NameAddr
obs_resent_send = obs_header "Resent-Sender" mailbox

-- |Parse a 'resent_date' header line but allow for the obsolete
-- folding syntax.

obs_resent_date :: Parser CalendarTime
obs_resent_date = obs_header "Resent-Date" date_time

-- |Parse a 'resent_to' header line but allow for the obsolete
-- folding syntax.

obs_resent_to   :: Parser [NameAddr]
obs_resent_to   = obs_header "Resent-To" mailbox_list

-- |Parse a 'resent_cc' header line but allow for the obsolete
-- folding syntax.

obs_resent_cc   :: Parser [NameAddr]
obs_resent_cc   = obs_header "Resent-Cc" mailbox_list

-- |Parse a 'resent_bcc' header line but allow for the obsolete
-- folding syntax.

obs_resent_bcc  :: Parser [NameAddr]
obs_resent_bcc  = obs_header "Bcc" (address_list <|> (cfws *> pure []))

-- |Parse a 'resent_msg_id' header line but allow for the obsolete
-- folding syntax.

obs_resent_mid  :: Parser ByteString
obs_resent_mid  = obs_header "Resent-Message-ID" msg_id

-- |Parse a @Resent-Reply-To@ header line but allow for the
-- obsolete folding syntax.

obs_resent_reply :: Parser [NameAddr]
obs_resent_reply = obs_header "Resent-Reply-To" address_list


-- ** Obsolete trace fields (section 4.5.7)

obs_return      :: Parser ByteString
obs_return       = obs_header "Return-Path" path

obs_received    :: Parser [(ByteString, ByteString)]
obs_received     = obs_header "Received" name_val_list

-- |Match 'obs_angle_addr'.

obs_path        :: Parser ByteString
obs_path        = obs_angle_addr

-- |This parser is identical to 'optional_field' but allows the more
-- liberal line-folding syntax between the \"field_name\" and the \"field
-- text\".

obs_optional    :: Parser (ByteString,Text)
obs_optional    = do n <- field_name
                     _ <- many wsp
                     _ <- char ':'
                     skipSpace
                     b <- unstructured
                     _ <- crlf
                     return (n,b)
                  <?> "optional (unspecified) header line"
