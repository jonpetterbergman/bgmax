{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  $Header$
Description :  Parser for the BgMax file format
Copyright   :  (c) Petter Bergman
License     :  BSD3

Maintainer  :  jon.petter.bergman@gmail.com
Stability   :  experimental
Portability :  non-portable (OverloadedStrings)

This module contains a Parser for the 
<http://www.bgc.se/upload/Gemensamt/Trycksaker/Manualer/BG6040.pdf BgMax>
format.

The parser tries to be forgiving with input it doesn't understand.
-}
module Finance.BgMax.Parser(parseBgMax) where
import Finance.BgMax.TK
import Data.ByteString.Char8(ByteString,dropWhile,reverse,readInt,readInteger,null,replicate)
import Data.Time(LocalTime(..),Day,TimeOfDay(..),fromGregorian)
import Data.Char(isSpace)
import Data.Attoparsec.ByteString.Char8(Parser,take,endOfLine,sepBy,string,skipWhile,notInClass)
import Prelude hiding (take,dropWhile,reverse,null,replicate)
import Control.Applicative((<|>),(*>),(<*>),(<$>),(<*))
import Data.Fixed(Pico)

class N a where
  readN :: ByteString -> Maybe (a,ByteString)

instance N Int where
  readN = readInt

instance N Integer where
  readN = readInteger
  
readFullN :: N a => ByteString -> Parser a
readFullN bs = 
  case readN bs of
    Nothing -> fail $ "could not read as Int: " ++ show bs
    Just (x,bs') | null bs' -> return x
                 | otherwise -> fail $ "could not read all characters as Int: " ++ show bs'

n' :: N a => ByteString -> Parser a
n' = readFullN

n :: N a => Int -> Parser a
n c = take c >>= n'

nhb' :: N a => ByteString -> Parser a
nhb' = readFullN . dropWhile isSpace

nhb :: N a => Int -> Parser a
nhb c = take c >>= nhb'

orBlank :: (Int -> Parser a) -> Int -> Parser (Maybe a)
orBlank p c = (fmap Just $ p c) <|> (string (replicate c ' ') *> (return Nothing))

avb' :: ByteString -> ByteString
avb' = reverse . dropWhile isSpace . reverse

avb :: Int -> Parser ByteString
avb = fmap avb' . take

ab' :: ByteString -> ByteString
ab' = dropWhile isSpace

ab :: Int -> Parser ByteString
ab = fmap ab' . take

pOrT :: Parser TestMark
pOrT =
  "T" *> return T <|>
  "P" *> return P

parseCurrency :: Parser Currency
parseCurrency =
  "EUR" *> return EUR <|>
  "SEK" *> return SEK

makeReference :: Int -> ByteString -> Parser Reference
makeReference 0 _ = return Ref0
makeReference 1 _ = return Ref1
makeReference 2 bs = fmap Ref2 $ nhb' bs
makeReference 3 bs = return $ Ref3 $ ab' bs
makeReference 4 bs = return $ Ref4 $ avb' bs
makeReference 5 bs = return $ Ref5 $ ab' bs
makeReference i bs = return $ Ref i bs

parseDepositType :: Parser DepositType
parseDepositType = "K" *> return K <|>
                   "D" *> return D <|>
                   "S" *> return S

parseDay :: Parser Day
parseDay = fromGregorian <$> n 4 <*> n 2 <*> n 2

parseMicroseconds :: Parser Pico
parseMicroseconds =
  do
    second <- n 2
    microsecond <- n 6
    return $ (fromInteger second) + ((fromInteger microsecond) / 1000000)

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = TimeOfDay <$> n 2 <*> n 2 <*> parseMicroseconds

localTime :: Parser LocalTime
localTime = LocalTime <$> parseDay <*> parseTimeOfDay

parseTransaction :: Parser Transaction
parseTransaction = 
  do
    sBG <- n 10
    ref' <- take 25
    amm <- n 18
    refCode <- n 1
    chan <- n 1
    bglNo <- n 12
    imag <- n 1
    ref <- makeReference refCode ref'
    return $ Transaction sBG ref amm chan bglNo imag

parsePost :: Parser a -> Parser b -> Parser b
parsePost code make = code *> make <* skipWhile (== ' ')

tk01 :: Parser Post
tk01 = fmap TK01 $ parsePost "01" $ TK01_t <$> avb 20 <*> n 2 <*> localTime <*> pOrT

tk05 :: Parser Post
tk05 = fmap TK05 $ parsePost "05" $ TK05_t <$> n 10 <*> orBlank n 10 <*> parseCurrency

tk20 :: Parser Post
tk20 = fmap TK20 $ parsePost "20" $ TK20_t <$> parseTransaction

tk21 :: Parser Post
tk21 = fmap TK21 $ parsePost "21" $ TK21_t <$> parseTransaction <*> n 1

tk22 :: Parser Post
tk22 = fmap TK22 $ parsePost "22" $ TK22_t <$> parseTransaction

tk23 :: Parser Post
tk23 = fmap TK23 $ parsePost "23" $ TK23_t <$> parseTransaction

tk25 :: Parser Post
tk25 = fmap TK25 $ parsePost "25" $ TK25_t <$> ab 50

tk26 :: Parser Post
tk26 = fmap TK26 $ parsePost "26" $ TK26_t <$> avb 35 <*> avb 35

tk27 :: Parser Post
tk27 = fmap TK27 $ parsePost "27" $ TK27_t <$> avb 35 <*> avb 9

tk28 :: Parser Post
tk28 = fmap TK28 $ parsePost "28" $ TK28_t <$> avb 35 <*> avb 35 <*> avb 2

tk29 :: Parser Post
tk29 = fmap TK29 $ parsePost "29" $ TK29_t <$> n 12

tk15 :: Parser Post
tk15 = fmap TK15 $ parsePost "15" $ TK15_t <$> n 35 <*> parseDay <*> n 5 <*> n 18 <*> parseCurrency <*> n 8 <*> orBlank (const parseDepositType) 1

tk70 :: Parser Post
tk70 = fmap TK70 $ parsePost "70" $ TK70_t <$> n 8 <*> n 8 <*> n 8 <*> n 8

tk :: Parser Post
tk = (TK <$> n 2) <* skipWhile (notInClass "\n\r")

-- | Parser for BgMax posts.
parseBgMax :: Parser [Post]
parseBgMax = sepBy go endOfLine 
  where go = tk01 <|> 
             tk05 <|> 
             tk20 <|> 
             tk21 <|> 
             tk22 <|> 
             tk23 <|> 
             tk25 <|>
             tk26 <|>
             tk27 <|>
             tk28 <|>
             tk29 <|>
             tk15 <|> 
             tk70 <|>
             tk