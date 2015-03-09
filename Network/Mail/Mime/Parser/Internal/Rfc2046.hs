{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      :  Network.Mail.Mime.Parser.Internal.Rfc2046
   Copyright   :  (c) 2015 Alberto Valverde
   License     :  BSD3

   Maintainer  :  alberto@toscat.net
   Stability   :  provisional
   Portability :  unknown

   This module provides parsers for the grammar defined in
   RFC2045, \"Multipurpose Internet Mail Extensions\",
   <https://tools.ietf.org/html/rfc2046>
-}

module Network.Mail.Mime.Parser.Internal.Rfc2046 where


import Control.Applicative (
  (<$>), (<*>), (*>), (<|>), pure, many, optional)
import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Base64 as B64
import Network.Mail.Mime.Parser.Internal.Common
import Network.Mail.Mime.Parser.Types
import Network.Mail.Mime.Parser.Internal.Rfc2234
import Network.Mail.Mime.Parser.Internal.Rfc2045
import Network.Mail.Mime.Parser.Internal.Rfc2822 hiding (body)
import Network.Mail.Mime.Parser.Internal.Rfc2183
import Network.Mail.Mime.Parser.Internal.Unicode
import Prelude hiding (takeWhile)

message :: Parser Message
message = do
  envelope
  hs <- mime_message_headers
  Message <$> pure hs <*> body hs endOfInput

envelope :: Parser ()
envelope = manyTill' (takeWhile1 (/='\r')) crlf *> pure ()

multipart_body :: ByteString -> Parser () -> Parser Body
multipart_body boundary endMarker = named "multipart_body" $ do
  pr <- preamble boundary
  parts <- many1 (part (multipart_separator boundary)) <?> "parts"
  ep <- epilogue endMarker <?> "epilogue"
  return $ MultipartBody pr parts ep

multipart_separator :: ByteString -> Parser ()
multipart_separator boundary = 
  let dash_boundary   = "--" *> string boundary <?> "dash_boundary"
      delimiter       = crlf *> dash_boundary   <?> "delimiter"
      close_delimiter = delimiter *> "--"       <?> "close_delimiter"
  in       (close_delimiter *> transport_padding *> pure ())
       <|> (delimiter *> transport_padding *> crlf *> pure ())
       <?> "multipart_separator"

preamble :: ByteString -> Parser ByteString
preamble boundary
  = binary_text_body
    ("--" *> string boundary *> transport_padding *> crlf *> pure ())
  <?> "preamble"

epilogue :: Parser () -> Parser ByteString
epilogue endMarker = binary_text_body endMarker <?> "epilogue"

mime_part_headers :: Parser [Field]
mime_part_headers = named "mime_part_headers" $ many mime_part_header

mime_part_header :: Parser Field
mime_part_header = content_disposition <|> entity_header <|> rfc2822_field

part :: Parser () -> Parser Part
part sep = named "part" $ do
  hs <- mime_part_headers
  Part <$> pure hs <*> body hs sep

body :: [Field] -> Parser () -> Parser Body
body hs end = named "body" $ do
  _ <- optional crlf
  bd <- case getContentType hs of
    ContentType "multipart" _ ps ->
      case getBoundary ps of
        Just b -> multipart_body b end
        _      -> fail "multipart content with no boundary"
    ContentType "text" _ ps -> do
      bd <- case getEncoding hs of
                 QuotedPrintable -> qp_body end
                 Base64          -> do bd <- fmap B64.decode (binary_body end)
                                       either fail return bd
                 _               -> binary_text_body end
      either fail (return . TextBody) $ toUnicode (S.unpack (getCharset ps)) bd
    _ -> do
      bd <- case getEncoding hs of
              QuotedPrintable -> qp_body end
              Base64          -> do bd <- fmap B64.decode (binary_body end)
                                    either fail return bd
              _               -> binary_body end
      return $ BinaryBody bd
  return bd

binary_body :: Parser () -> Parser ByteString
binary_body = fmap S.concat . takeLines

binary_text_body :: Parser () -> Parser ByteString
binary_text_body = fmap unlines' . takeLines
  where unlines' (x:[]) = x <> "\n"
        unlines' l      = S.intercalate "\n" l

-- takeLines until endMarker is found.
-- must handle blank empty lines between valid lines but not before endMarker
-- since the endMarker might start with a crlf
takeLines :: Parser () -> Parser [ByteString]
takeLines endMarker = go
  where
    go = (endMarker *> pure [])
     <|> (do l <- takeWhile1 (/='\r')
             (endMarker *> pure [l]) <|> (crlf *> fmap (l:) go))
     <|> (crlf *> (("":) <$> go))

qp_body :: Parser () -> Parser ByteString
qp_body sep = do
  s <- S.intercalate "\r\n" <$> takeLines sep
  either fail return $ parseOnly quoted_printable s
