{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Text.ParserCombinators.AttoParsec.Rfc2821
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  unknown

   This module exports parser combinators for the grammar
   described in RFC2821, \"Simple Mail Transfer Protocol\",
   <http://www.faqs.org/rfcs/rfc2821.html>.
-}

module Text.ParserCombinators.Attoparsec.Rfc2821 where

import Control.Applicative (many, pure, (<$>), (<|>))
import Control.Exception ( assert )
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid ((<>))
import Data.Char ( toLower )
import Text.ParserCombinators.Attoparsec.Rfc2234
import Text.ParserCombinators.Attoparsec.ParsecCompat

----------------------------------------------------------------------
-- * ESMTP State Machine
----------------------------------------------------------------------

data SessionState
  = Unknown
  | HaveHelo
  | HaveMailFrom
  | HaveRcptTo
  | HaveData
  | HaveQuit
  deriving (Enum, Bounded, Eq, Ord, Show)

data Event
  = Greeting                    -- ^ reserved for the user
  | SayHelo       ByteString
  | SayHeloAgain  ByteString
  | SayEhlo       ByteString
  | SayEhloAgain  ByteString
  | SetMailFrom   Mailbox
  | AddRcptTo     Mailbox
  | StartData
  | Deliver                     -- ^ reserved for the user
  | NeedHeloFirst
  | NeedMailFromFirst
  | NeedRcptToFirst
  | NotImplemened
        -- ^ 'Turn', 'Send', 'Soml', 'Saml', 'Vrfy', and 'Expn'.
  | ResetState
  | SayOK
        -- ^ Triggered in case of 'Noop' or when 'Rset' is
        -- used before we even have a state.
  | SeeksHelp     ByteString
        -- ^ The parameter may be @[]@.
  | Shutdown
  | SyntaxErrorIn ByteString
  | Unrecognized  ByteString
  deriving (Eq, Show)

type SmtpdFSM = Control.Monad.State.State SessionState Event

-- |Parse a line of SMTP dialogue and run 'handleSmtpCmd' to
-- determine the 'Event'. In case of syntax errors,
-- 'SyntaxErrorIn' or 'Unrecognized' will be returned.
-- Inputs must be terminated with 'crlf'. See 'fixCRLF'.

smtpdFSM :: ByteString -> SmtpdFSM
smtpdFSM str = either
                 (\_ -> return (Unrecognized str))
                 handleSmtpCmd
                 (parseOnly smtpCmd str)

-- |For those who want to parse the 'SmtpCmd' themselves.
-- Calling this function in 'HaveQuit' or 'HaveData' will
-- fail an assertion. If 'assert' is disabled, it will
-- return respectively 'Shutdown' and 'StartData' again.

handleSmtpCmd :: SmtpCmd -> SmtpdFSM
handleSmtpCmd cmd = get >>= \st -> match' st cmd
  where
  match' :: SessionState -> SmtpCmd -> SmtpdFSM
  match' HaveQuit     _       = assert False (event Shutdown)
  match' HaveData     _       = assert False (trans (HaveData, StartData))
  match'    _  (WrongArg c)   = event (SyntaxErrorIn c)
  match'    _        Quit     = trans (HaveQuit, Shutdown)
  match'    _        Noop     = event SayOK
  match'    _        Turn     = event NotImplemened

  match'    _      (Send _)   = event NotImplemened
  match'    _      (Soml _)   = event NotImplemened
  match'    _      (Saml _)   = event NotImplemened
  match'    _      (Vrfy _)   = event NotImplemened
  match'    _      (Expn _)   = event NotImplemened
  match'    _      (Help x)   = event (SeeksHelp x)

  match' Unknown    Rset      = event SayOK
  match' HaveHelo   Rset      = event SayOK
  match'    _       Rset      = trans (HaveHelo, ResetState)

  match' Unknown   (Helo x)   = trans (HaveHelo, SayHelo x)
  match'    _      (Helo x)   = trans (HaveHelo, SayHeloAgain x)
  match' Unknown   (Ehlo x)   = trans (HaveHelo, SayEhlo x)
  match'    _      (Ehlo x)   = trans (HaveHelo, SayEhloAgain x)

  match' Unknown (MailFrom _) = event NeedHeloFirst
  match'    _    (MailFrom x) = trans (HaveMailFrom, SetMailFrom x)

  match' Unknown  (RcptTo _)  = event NeedHeloFirst
  match' HaveHelo (RcptTo _)  = event NeedMailFromFirst
  match'    _     (RcptTo x)  = trans (HaveRcptTo, AddRcptTo x)

  match' Unknown       Data   = event NeedHeloFirst
  match' HaveHelo      Data   = event NeedMailFromFirst
  match' HaveMailFrom  Data   = event NeedRcptToFirst
  match' HaveRcptTo    Data   = trans (HaveData, StartData)

  event :: Event -> SmtpdFSM
  event = return

  trans :: (SessionState, Event) -> SmtpdFSM
  trans (st,e) = put st >> event e


----------------------------------------------------------------------
-- * Data Types for SMTP Commands
----------------------------------------------------------------------

-- |The 'smtpCmd' parser will create this data type from a
-- string. Note that /all/ command parsers expect their
-- input to be terminated with 'crlf'.

data SmtpCmd
  = Helo ByteString
  | Ehlo ByteString
  | MailFrom Mailbox            -- ^ Might be 'nullPath'.
  | RcptTo Mailbox              -- ^ Might be 'postmaster'.
  | Data
  | Rset
  | Send Mailbox
  | Soml Mailbox
  | Saml Mailbox
  | Vrfy ByteString
  | Expn ByteString
  | Help ByteString                 -- ^ Might be @[]@.
  | Noop                        -- ^ Optional argument ignored.
  | Quit
  | Turn
  | WrongArg ByteString
      -- ^ When a valid command has been recognized, but the
      -- argument parser fails, then this type will be
      -- returned. The 'ByteString' contains the name of the
      -- command (in all upper-case).

instance Show SmtpCmd where
  show (Helo str)    = "HELO " ++ S.unpack str
  show (Ehlo str)    = "EHLO " ++ S.unpack str
  show (MailFrom mbox) = "MAIL FROM:" ++ show mbox
  show (RcptTo mbox) = "RCPT TO:" ++ show mbox
  show (Data)        = "DATA"
  show (Rset)        = "RSET"
  show (Send mbox)   = "SEND " ++ show mbox
  show (Soml mbox)   = "SOML " ++ show mbox
  show (Saml mbox)   = "SAML " ++ show mbox
  show (Vrfy str)    = "VRFY " ++ S.unpack str
  show (Expn str)    = "EXPN " ++ S.unpack str
  show (Noop)        = "NOOP"
  show (Quit)        = "QUIT"
  show (Turn)        = "TURN"
  show (Help t)
    | S.null t       = "HELP"
    | otherwise      = "HELP " ++ S.unpack t
  show (WrongArg str) =
    "Syntax error in argument of " ++ S.unpack str ++ "."

-- |The most general e-mail address has the form:
-- @\<[\@route,...:]user\@domain\>@. This type, too,
-- supports 'show'. Note that a \"shown\" address
-- is /always/ enclosed in angular brackets. When comparing
-- two mailboxes for equality, the hostname is case-insensitive.

data Mailbox = Mailbox [ByteString] ByteString ByteString

instance Eq Mailbox where
  lhs == rhs  =  norm lhs == norm rhs
    where
    norm (Mailbox rt lp hp) = (rt, lp, S.map toLower hp)

instance Show Mailbox where
  show (Mailbox [] "" "") = "<>"
  show (Mailbox [] "postmaster" "") = "<postmaster>"
  show (Mailbox p u d) = "<" ++ route ++ (if null route then [] else ":") ++ mbox ++ ">"
    where
      route = S.unpack $ S.intercalate "," . map (S.cons '@') $ p
      mbox  = S.unpack $ u <> "@" <> d

-- |@nullPath@ @=@ @'Mailbox' [] \"\" \"\" = \"\<\>\"@

nullPath :: Mailbox
nullPath = Mailbox [] "" ""

-- |@postmaster@ @=@ @'Mailbox' [] \"postmaster\" \"\" = \"\<postmaster\>\"@

postmaster :: Mailbox
postmaster = Mailbox [] "postmaster" ""


----------------------------------------------------------------------
-- * Data Types for SMTP Replies
----------------------------------------------------------------------

-- |An SMTP reply is a three-digit return code plus some waste of
-- bandwidth called \"comments\". This is what the list of strings is
-- for; one string per line in the reply. 'show' will append an
-- \"@\\r\\n@\" end-of-line marker to each entry in that list, so that
-- the resulting string is ready to be sent back to the peer. For
-- example:
--
-- >>> show $ Reply (Code Success MailSystem 0) ["worked", "like", "a charm" ]
-- "250-worked\r\n250-like\r\n250 a charm\r\n"
--
-- If the message is an empty list @[]@, a default text will be constructed:
--
-- >>> show $ Reply (Code Success MailSystem 0) []
-- "250 Success in category MailSystem\r\n"

data SmtpReply = Reply SmtpCode [ByteString]

data SmtpCode = Code SuccessCode Category Int

data SuccessCode
  = Unused0
  | PreliminarySuccess
  | Success
  | IntermediateSuccess
  | TransientFailure
  | PermanentFailure
  deriving (Enum, Bounded, Eq, Ord, Show)

data Category
  = Syntax
  | Information
  | Connection
  | Unspecified3
  | Unspecified4
  | MailSystem
  deriving (Enum, Bounded, Eq, Ord, Show)

instance Show SmtpReply where
  show (Reply c@(Code suc cat _) []) =
    let msg = S.pack $ show suc ++ " in category " ++ show cat
    in
    show $ Reply c [msg]

  show (Reply code msg) =
    let prefixCon = show code ++ "-"
        prefixEnd = show code ++ " "
        fmt p l   = p ++ l ++ "\r\n"
        (x:xs) = map S.unpack $ reverse msg
        msgCon = map (fmt prefixCon) xs
        msgEnd = fmt prefixEnd x
        msg'   = reverse (msgEnd:msgCon)
    in
    concat msg'

instance Show SmtpCode where
  show (Code suc cat n) =
    assert (n >= 0 && n <= 9) $
      (show . fromEnum) suc ++ (show . fromEnum) cat ++ show n

-- |Construct a 'Reply'. Fails 'assert' if invalid numbers
-- are given.

reply :: Int -> Int -> Int -> [ByteString] -> SmtpReply
reply suc c n msg =
  assert (suc >= 0 && suc <= 5) $
    assert (c >= 0 && c <= 5)   $
      assert (n >= 0 && n <= 9) $
        Reply (Code (toEnum suc) (toEnum c) n) msg

-- |A reply constitutes \"success\" if the status code is
-- any of 'PreliminarySuccess', 'Success', or
-- 'IntermediateSuccess'.

isSuccess :: SmtpReply -> Bool
isSuccess (Reply (Code PreliminarySuccess _ _) _)  = True
isSuccess (Reply (Code Success _ _) _)             = True
isSuccess (Reply (Code IntermediateSuccess _ _) _) = True
isSuccess _                                        = False

-- |A reply constitutes \"failure\" if the status code is
-- either 'PermanentFailure' or 'TransientFailure'.

isFailure :: SmtpReply -> Bool
isFailure (Reply (Code PermanentFailure _ _) _) = True
isFailure (Reply (Code TransientFailure _ _) _) = True
isFailure _                                     = False

-- |The replies @221@ and @421@ signify 'Shutdown'.

isShutdown :: SmtpReply -> Bool
isShutdown (Reply (Code Success Connection 1) _)          = True
isShutdown (Reply (Code TransientFailure Connection 1) _) = True
isShutdown _                                              = False

----------------------------------------------------------------------
-- * Command Parsers
----------------------------------------------------------------------

-- |The SMTP parsers defined here correspond to the commands
-- specified in RFC2821, so I won't document them
-- individually.

type SmtpParser st = Parser SmtpCmd

-- |This parser recognizes any of the SMTP commands defined
-- below. Note that /all/ command parsers expect their input
-- to be terminated with 'crlf'.

smtpCmd :: SmtpParser st
smtpCmd = choice
          [ smtpData, rset, noop, quit, turn
          , helo, mail, rcpt, send, soml, saml
          , vrfy, expn, help, ehlo
          ]

-- |The parser name \"data\" was taken.
smtpData :: SmtpParser st
rset, quit, turn, helo, ehlo, mail :: SmtpParser st
rcpt, send, soml, saml, vrfy, expn :: SmtpParser st
help                               :: SmtpParser st

-- |May have an optional 'word' argument, but it is ignored.
noop :: SmtpParser st

smtpData = mkCmd0 "DATA" Data
rset = mkCmd0 "RSET" Rset
quit = mkCmd0 "QUIT" Quit
turn = mkCmd0 "TURN" Turn
helo = mkCmd1 "HELO" Helo     domain
ehlo = mkCmd1 "EHLO" Ehlo     domain
mail = mkCmd1 "MAIL" MailFrom from_path
rcpt = mkCmd1 "RCPT" RcptTo   to_path
send = mkCmd1 "SEND" Send     from_path
soml = mkCmd1 "SOML" Soml     from_path
saml = mkCmd1 "SAML" Saml     from_path
vrfy = mkCmd1 "VRFY" Vrfy     word
expn = mkCmd1 "EXPN" Expn     word

help = try (mkCmd0 "HELP" (Help "")) <|>
       mkCmd1 "HELP" Help (option "" word)

noop = try (mkCmd0 "NOOP" Noop) <|>
       mkCmd1 "NOOP" (const Noop) (option "" word)


----------------------------------------------------------------------
-- * Argument Parsers
----------------------------------------------------------------------

from_path :: Parser Mailbox
from_path = do
  _ <- stringCI "from:"
  (try (string "<>" >> return nullPath) <|> path)
                                <?> "from-path"

to_path :: Parser Mailbox
to_path = do
  _ <- stringCI "to:"
  (try (stringCI "<postmaster>" >> return postmaster)
     <|> path)                  <?> "to-path"

path :: Parser Mailbox
path = between (char '<') (char '>') (p <?> "path")
  where
  p = do
    r1 <- option [] (a_d_l >>= \r -> char ':' >> return r)
    (Mailbox _ l d) <- mailbox
    return (Mailbox r1 l d)

mailbox :: Parser Mailbox
mailbox = p <?> "mailbox"
  where
  p = do
    r1 <- local_part
    _ <- char '@'
    r2 <- domain
    return (Mailbox [] r1 r2)

local_part :: Parser ByteString
local_part = (dot_string <|> quoted_string) <?> "local-part"

domain :: Parser ByteString
domain = choice
         [ tokenList subdomain '.'  <?> "domain"
         , address_literal          <?> "address literal"
         ]

a_d_l :: Parser [ByteString]
a_d_l = sepBy1 at_domain (char ',') <?> "route-list"

at_domain :: Parser ByteString
at_domain = (char '@' >> domain) <?> "at-domain"

-- |/TODO/: Add IPv6 address and general literals
address_literal :: Parser ByteString
address_literal = ipv4_literal  <?> "IPv4 address literal"

ipv4_literal :: Parser ByteString
ipv4_literal = do
  rs <- between (char '[') (char ']') ipv4addr
  return ('[' `S.cons` S.reverse (']' `S.cons` S.reverse rs))

ipv4addr :: Parser ByteString
ipv4addr = p <?> "IPv4 address literal"
  where
  p = do
    r1 <- snum
    r2 <- char '.' >> snum
    r3 <- char '.' >> snum
    r4 <- char '.' >> snum
    return (r1 <> "." <> r2 <> "." <> r3 <> "." <> r4)

subdomain :: Parser ByteString
subdomain = p <?> "domain name"
  where
  p = do
    r <- takeWhile1 (\c -> isAsciiAlpha c || isDigit c || c=='-')
    if S.last r == '-'
        then fail "subdomain must not end with hyphen"
        else return r

dot_string :: Parser ByteString
dot_string = tokenList atom '.' <?> "dot_string"

atom :: Parser ByteString
atom = takeWhile1 (\c -> isAsciiAlpha c 
                             || isDigit c
                             || c `elem` "!#$%&'*+-/=?^_`{|}~") <?> "atom"

snum :: Parser ByteString
snum = do
  (r',r) <- match (readIntN 3 <|> readIntN 2 <|> readIntN 1)
  if r > 255
     then fail "IP address parts must be 0 <= x <= 255"
     else return r'

number :: Parser ByteString
number = takeWhile1 isDigit

-- |This is a useful addition: The parser accepts an 'atom'
-- or a 'quoted_string'.

word :: Parser ByteString
word = (atom <|> quoted_string)
       <?> "word or quoted-string"


----------------------------------------------------------------------
-- * Helper Functions
----------------------------------------------------------------------

-- |Make the string 'crlf' terminated no matter what.
-- \'@\\n@\' is expanded, otherwise 'crlf' is appended. Note
-- that if the string was terminated incorrectly before, it
-- still is. This function is useful when reading input with
-- 'System.IO.hGetLine' which removes the end-of-line
-- delimiter.

fixCRLF :: ByteString -> ByteString
fixCRLF "\r\n" = fixCRLF ""
fixCRLF s
  | S.length s == 2 && S.last s == '\n' = S.head s `S.cons` fixCRLF ""
  | S.null s                            = "\r\n"
  | otherwise                           = S.head s `S.cons` fixCRLF (S.tail s)

-- |Construct a parser for a command without arguments.
-- Expects 'crlf'!

mkCmd0 :: ByteString -> a -> Parser a
mkCmd0 str cons = (do
  _ <- stringCI str
  _ <- skipMany wsp >> crlf
  return cons)                          <?> show str

-- |Construct a parser for a command with an argument, which
-- the given parser will handle. The result of the argument
-- parser will be applied to the type constructor before it
-- is returned. Expects 'crlf'!

mkCmd1 :: ByteString -> (a -> SmtpCmd) -> Parser a
       -> Parser SmtpCmd
mkCmd1 str cons p = do
  _ <- stringCI str
  _ <- wsp
  let eol = skipMany wsp >> crlf
      p'  = between (many wsp) eol p <?>  show str
  (cons <$> p') <|> pure (WrongArg str)

-- @tokenList p '.'@ will parse a token of the form
-- \"@p.p@\", or \"@p.p.p@\", and so on. Used in 'domain'
-- and 'dot_string', for example.

tokenList :: Parser ByteString -> Char
          -> Parser ByteString
tokenList p c = fmap (S.intercalate (S.singleton c)) (sepBy1 p (char c))
