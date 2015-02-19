{-# LANGUAGE TemplateHaskell #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Types
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

 -}

module Network.Mail.Mime.Parser.Types (
  -- |Types
    Message (..)
  , Field (..)
  , Part (..)
  , Parameter (..)
  , Encoding (..)
  , NameAddr (..)

  -- |Lenses
  , msgBody
  , msgHeaders
  , nameAddr_addr
  , nameAddr_name
  , cdDisposition
  , cdFilename
  , ctMime
  , ctParam
  , partContent
  , partEncoding
  , partFilename
  , partHeaders
  , partType

  -- |Prisms
  , _OtherField
  , _From
  , _Sender
  , _ReturnPath
  , _ReplyTo
  , _To
  , _Cc
  , _Bcc
  , _MessageID
  , _InReplyTo
  , _References
  , _Subject
  , _Comments
  , _Keywords
  , _Date
  , _ResentDate
  , _ResentFrom
  , _ResentSender
  , _ResentTo
  , _ResentCc
  , _ResentBcc
  , _ResentMessageID
  , _ResentReplyTo
  , _Received
  , _ObsReceived
  , _ContentType
  , _ContentLength
  , _ContentTransferEncoding
  , _ContentDisposition

  , _Boundary
  , _Name
  , _Charset
  , _OtherParameter
) where

import System.Time (CalendarTime)
import Control.Lens
import Data.ByteString (ByteString)

-- |This data type repesents a parsed Internet Message.
-- It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data Message
  = Message {
        _msgHeaders :: [Field]
      , _msgBody    :: Either ByteString [Part]
    } deriving Show

data Field
  -- | RFC2822 fields
  = OtherField          ByteString ByteString
  | From                [NameAddr]
  | Sender              NameAddr
  | ReturnPath          ByteString
  | ReplyTo             [NameAddr]
  | To                  [NameAddr]
  | Cc                  [NameAddr]
  | Bcc                 [NameAddr]
  | MessageID           ByteString
  | InReplyTo           [ByteString]
  | References          [ByteString]
  | Subject             ByteString
  | Comments            ByteString
  | Keywords            [[ByteString]]
  | Date                CalendarTime
  | ResentDate          CalendarTime
  | ResentFrom          [NameAddr]
  | ResentSender        NameAddr
  | ResentTo            [NameAddr]
  | ResentCc            [NameAddr]
  | ResentBcc           [NameAddr]
  | ResentMessageID     ByteString
  | ResentReplyTo       [NameAddr]
  | Received            ([(ByteString,ByteString)], CalendarTime)
  | ObsReceived         [(ByteString,ByteString)]

  -- | RFC2045 fields
  --
  | ContentType {
        _ctMime  :: ByteString
      , _ctParam :: Maybe Parameter
      }
  | ContentLength Integer
  | ContentTransferEncoding Encoding
  | ContentDisposition {
        _cdDisposition :: ByteString
      , _cdFilename    :: Maybe ByteString
      }
  deriving (Show, Eq)
--
-- |A NameAddr is composed of an optional realname a mandatory
-- e-mail 'address'.
data NameAddr
  = NameAddr { _nameAddr_name :: Maybe ByteString
             , _nameAddr_addr :: ByteString }
  deriving (Show,Eq)


data Part
  = Part {
        _partType     :: ByteString
      , _partEncoding :: Encoding
      , _partFilename :: Maybe ByteString
      , _partHeaders  :: [Field]
      , _partContent  :: ByteString
      }
  deriving Show

data Parameter
  = Boundary       ByteString
  | Name           ByteString
  | Charset        ByteString
  | OtherParameter ByteString ByteString
  deriving (Show, Eq)


data Encoding
  = NoEncoding
  | Base64
  | QuotedPrintable
  | Binary
  | Binary8Bit
  | Binary7Bit
  | OtherEncoding ByteString
  deriving (Eq, Show)

makeLenses ''Message
makeLenses ''NameAddr
makeLenses ''Field
makePrisms ''Field
makeLenses ''Part
makePrisms ''Parameter
