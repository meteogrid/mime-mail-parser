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
  , MultipartBody (..)
  , Field (..)
  , Part (..)
  , ContentTypeParm (..)
  , ContentDispositionParm (..)
  , ContentDispositionType (..)
  , Encoding (..)
  , NameAddr (..)

  -- |Lenses
  , msgHeaders
  , msgBody
  , msgMultipartBody
  , nameAddr_addr
  , nameAddr_name
  , cdType
  , cdParms
  , ctType
  , ctSubtype
  , ctParms
  , partHeaders
  , partBody
  , mpPreamble
  , mpEpilogue
  , mpParts
  , mimeExtName
  , mimeExtValue
  , mimeVerMajor
  , mimeVerMinor

  -- |Prisms
  , _Message
  , _MultipartMessage
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
  , _ContentTypeParm
  
  , _Filename
  , _CreationDate
  , _ModificationDate
  , _ReadDate
  , _Size
  , _ContentDispositionParm
) where

import System.Time (CalendarTime)
import Control.Lens
import Data.ByteString (ByteString)

-- |This data type repesents a parsed Internet Message.
-- It consists of an arbitrary number of header lines,
-- represented in the 'Field' data type, and a message body, which may
-- be empty.

data Message
  = Message
      { _msgHeaders :: [Field]
      , _msgBody    :: ByteString
      }
  | MultipartMessage
      { _msgHeaders       :: [Field]
      , _msgMultipartBody :: MultipartBody
      }
  deriving Show

data MultipartBody
  = MultipartBody {
        _mpPreamble :: ByteString
      , _mpParts    :: [Part]
      , _mpEpilogue :: ByteString
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
  | ContentType {
        _ctType    :: ByteString
      , _ctSubtype :: ByteString
      , _ctParms   :: [ContentTypeParm]
      }
  | ContentDescription      ByteString
  | ContentLength           Integer
  | ContentTransferEncoding Encoding
  | ContentDisposition {
        _cdType  :: ContentDispositionType
      , _cdParms :: [ContentDispositionParm]
      }
  | MimeExtension {
        _mimeExtName  :: ByteString
      , _mimeExtValue :: ByteString
      }
  | MimeVersion {
        _mimeVerMajor :: Int
      , _mimeVerMinor :: Int
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
        _partHeaders :: [Field]
      , _partBody    :: ByteString
      }
  deriving Show

data ContentTypeParm
  = Boundary         ByteString
  | Name             ByteString
  | Charset          ByteString
  | ContentTypeParm  ByteString ByteString
  deriving (Show, Eq)

data ContentDispositionType
  = Inline
  | Attachment
  | ContentDispositionType ByteString
  deriving (Show, Eq)

data ContentDispositionParm
  = Filename                ByteString
  | CreationDate            CalendarTime
  | ModificationDate        CalendarTime
  | ReadDate                CalendarTime
  | Size                    Integer
  | ContentDispositionParm  ByteString ByteString
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
makePrisms ''Message
makeLenses ''MultipartBody
makeLenses ''NameAddr
makeLenses ''Field
makePrisms ''Field
makeLenses ''Part
makePrisms ''ContentTypeParm
makePrisms ''ContentDispositionType
makePrisms ''ContentDispositionParm
