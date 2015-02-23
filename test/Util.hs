module Util (parseTest, parseIdemTest, parseFailure) where

import Test.Hspec (Expectation, shouldReturn, shouldSatisfy)
import Control.Applicative ((<*))
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, endOfInput)
import Data.ByteString.Char8 (ByteString)


parseTest :: Parser a -> ByteString -> IO a
parseTest p input = case parseOnly (p <* endOfInput) input of
                      Left err -> fail ("parse error at " ++ err)
                      Right r -> return r

parseIdemTest :: Parser ByteString -> ByteString -> Expectation
parseIdemTest p input = parseTest p input `shouldReturn` input

parseFailure :: (Show a) => Parser a -> ByteString -> Expectation
parseFailure p input = parseOnly (p <* endOfInput) input `shouldSatisfy` failure
  where
    failure (Left _) = True
    failure _        = False
