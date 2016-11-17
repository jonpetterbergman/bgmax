{-# language TemplateHaskell #-}
module Test where

import BankGiro.BgMax

import Data.Text.Encoding

import Data.Aeson
import Data.Aeson.TH

import Data.Yaml.Pretty

import Data.ByteString.Char8(ByteString,unpack)
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString(Parser,parseOnly)


instance ToJSON ByteString where
  toJSON bs = String $ decodeLatin1 bs

deriveToJSON defaultOptions ''PaymentChannel                                        
                                        
deriveToJSON defaultOptions ''Image

deriveToJSON defaultOptions ''Address

deriveToJSON defaultOptions ''Sender                                          

deriveToJSON defaultOptions ''Reference

deriveToJSON defaultOptions ''DeductionCode

deriveToJSON defaultOptions ''EntryType                                     

deriveToJSON defaultOptions ''Entry                                    

deriveToJSON defaultOptions ''Currency

deriveToJSON defaultOptions ''DepositType

deriveToJSON defaultOptions ''Section

deriveToJSON defaultOptions ''TestMark

deriveToJSON defaultOptions ''BgMax
  
parseTestOnly :: ToJSON a => Parser a -> ByteString -> IO ()
parseTestOnly p s = either print (putStrLn . unpack . encodePretty defConfig) $ parseOnly p s

testFile :: ToJSON a => Parser a -> FilePath -> IO ()
testFile p f = BS.readFile f >>= parseTestOnly p