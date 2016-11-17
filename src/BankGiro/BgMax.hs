{-# LANGUAGE OverloadedStrings #-}
module BankGiro.BgMax where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Data.Attoparsec.ByteString       (Parser, eitherP, endOfInput,
                                                   many', many1, option,
                                                   parseOnly, (<?>))
import qualified Data.Attoparsec.ByteString       as Atto
import           Data.Attoparsec.ByteString.Char8 (IResult (..), anyChar,
                                                   endOfLine, manyTill, parse,
                                                   string, takeTill)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Char8            (snoc, unpack)
import           Data.Fixed                       (Centi, Fixed (..))
import           Data.Time                        (Day, LocalTime)
import           Data.Time.Format                 (defaultTimeLocale,
                                                   parseTimeM)

data PaymentChannel =
    ElectronicBank
  | ElectronicLB
  | Paper
  | ElectronicAG
  | PaymentChannelReserved Int deriving (Show,Eq)

data Image =
    NoImage
  | Image
  | ImageReserved Int deriving (Show,Eq)

data Address =
  Address {
    street      :: Maybe ByteString
  , zipCode     :: Maybe ByteString
  , city        :: Maybe ByteString
  , country     :: Maybe ByteString
  , countryCode :: Maybe ByteString } deriving Show

data Sender =
  Sender {
    senderBankGiro :: Integer
  , senderName     :: Maybe (ByteString,ByteString)
  , address        :: Address
  , orgNo          :: Maybe Integer } deriving Show

data Reference =
    Blank
  | BlankNoUB
  | CorrectOCR Integer Centi
  | IncorrectOCR ByteString
  | CorrectUB ByteString Centi
  | IncorrectUB ByteString
  | ExtendedRef Int ByteString deriving Show

mkReference :: Int -> ByteString -> Centi -> Reference
mkReference 0 _  _ = Blank
mkReference 1 _  _ = BlankNoUB
mkReference 2 bs x = CorrectOCR (read $ unpack bs) x
mkReference 3 bs _ = IncorrectOCR bs
mkReference 4 bs x = CorrectUB bs x
mkReference 5 bs _ = IncorrectUB bs
mkReference n bs _ = ExtendedRef n bs

data DeductionCode =
    WholeNoRest
  | PartialRest
  | PartialFinal
  | DeductionCodeReserved Int deriving Show

mkDeductionCode :: Int -> DeductionCode
mkDeductionCode 1 = WholeNoRest
mkDeductionCode 2 = PartialRest
mkDeductionCode 3 = PartialFinal
mkDeductionCode n = DeductionCodeReserved n

data EntryType =
    Payment
  | Deduction DeductionCode deriving Show

data Entry =
  Entry {
    entryType      :: EntryType
  , sender         :: Sender
  , reference      :: [Reference]
  , ammount        :: Centi
  , paymentChannel :: PaymentChannel
  , bgSerialNo     :: Integer
  , image          :: Image
  , info           :: [ByteString] } deriving Show

data Currency = SEK deriving Show

data DepositType = K | D | S deriving (Show,Read)

data Section =
  Section {
    recipientBankGiro :: Integer
  , recipientPostGiro :: Maybe Integer
  , recipientAccount  :: Integer
  , depositDay        :: Day
  , depositNo         :: Integer
  , depositAmmount    :: Centi
  , sNoPayments       :: Integer
  , depositType       :: Maybe DepositType
  , currency          :: Currency
  , entries           :: [Entry] } deriving Show

data TestMark = T | P deriving Show

data BgMax =
  BgMax {
    version           :: Int
  , writeDay          :: LocalTime
  , testMark          :: TestMark
  , noPayments        :: Integer
  , noDeductions      :: Integer
  , noExtraReferences :: Integer
  , noDeposits        :: Integer
  , sections          :: [Section] } deriving Show

mkPaymentChannel  :: Int -> PaymentChannel
mkPaymentChannel 1 = ElectronicBank
mkPaymentChannel 2 = ElectronicLB
mkPaymentChannel 3 = Paper
mkPaymentChannel 4 = ElectronicAG
mkPaymentChannel n = PaymentChannelReserved n

mkImage :: Int -> Image
mkImage 0 = NoImage
mkImage 1 = Image
mkImage n = ImageReserved n


assert :: Bool -> String -> Parser ()
assert cond msg | cond = return ()
                | otherwise = fail msg

bgMax :: Parser BgMax
bgMax =
  do
    (v,ts,tm) <- startPost
    sects <- many1 section
    (noPayments,noDeductions,noExtraReferences,noDeposits) <- endPost
    return $ BgMax v ts tm noPayments noDeductions noExtraReferences noDeposits sects

isEndOfLine :: Char -> Bool
isEndOfLine '\n' = True
isEndOfLine '\r' = True
isEndOfLine _    = False

dummyLine :: ByteString -> Parser ()
dummyLine x = string x >> tillEol >> return ()

tillEol :: Parser String
tillEol = manyTill anyChar (endOfLine <|> endOfInput)

takeTab :: ByteString -> Int -> Int -> Parser ByteString
takeTab acc _      0 = return acc
takeTab acc tabLen n | n > 0 =
  do
    c <- anyChar
    takeTab (acc `snoc` c) tabLen (n - (if (c == '\t') then tabLen else 1))
                     | otherwise = mempty

myTake :: Int -> Parser ByteString
myTake n = takeTab "" 6 n

takeT :: Int -> Parser ByteString
takeT n =
  do
    c <- myTake n
    return c

takeReadMaybe :: Read a => Int -> Parser (Maybe a)
takeReadMaybe n =
  do
    c <- myTake n
    case reads $ unpack c of
      [(x,_)] -> return $ Just x
      _       -> return Nothing

takeRead :: Read a => Int -> Parser a
takeRead n =
  do
    it <- takeReadMaybe n
    maybe mempty return it

pTestMark :: Parser TestMark
pTestMark = (string "P" >> return P) <|> (string "T" >> return T)

pCurrency :: Parser Currency
pCurrency = string "SEK" >> return SEK

timeStamp :: Parser LocalTime
timeStamp =
  do
    str <- fmap unpack $ myTake 20
    parseTimeM False defaultTimeLocale "%_Y%m%d%H%M%S%q" $ str ++ "000000"

dateStamp :: Parser Day
dateStamp =
  do
    str <- fmap unpack $ myTake 8
    parseTimeM False defaultTimeLocale "%_Y%m%d" str


startPost :: Parser (Int,LocalTime,TestMark)
startPost =
  (do
     string "01BGMAX               "
     v <- takeRead 2
     ts <- timeStamp
     tm <- pTestMark
     tillEol
     return (v,ts,tm)) <?> "startPost"

endPost :: Parser (Integer,Integer,Integer,Integer)
endPost =
  (do
     string "70"
     noPayments <- takeRead 8
     noDeductions <- takeRead 8
     noExtraReferences <- takeRead 8
     noDeposits <- takeRead 8
     tillEol
     return (noPayments,noDeductions,noExtraReferences,noDeposits)) <?> "endPost"

openingPost :: Parser (Integer,Maybe Integer,Currency)
openingPost =
  (do
     string "05"
     bgNo <- takeRead 10
     pgNo <- (fmap Just $ takeRead 10) <|> (myTake 10 >> return Nothing)
     c <- pCurrency
     tillEol
     return (bgNo,pgNo,c)) <?> "openingPost"

depositPost :: Parser (Integer,Day,Integer,Centi,Currency,Integer,Maybe DepositType)
depositPost =
  (do
     string "15"
     accNo <- takeRead 35
     payDay <- dateStamp
     depositNo <- takeRead 5
     amm <- fmap MkFixed $ takeRead 18
     c <- pCurrency
     noPayments <- takeRead 8
     depositType <- takeReadMaybe 1
     tillEol
     return (accNo,payDay,depositNo,amm,c,noPayments,depositType)) <?> "depositPost"

section :: Parser Section
section =
  (do
     (rBgNo,rPgNo,c) <- openingPost
     ents <- many1 entry
     (accNo,payDay,depositNo,amm,c',noPayments,depositType) <- depositPost
     return $ Section rBgNo rPgNo accNo payDay depositNo amm noPayments depositType c ents) <?> "section"

entry :: Parser Entry
entry = deduction <|> payment

maybeP :: Parser a -> Parser (Maybe a)
maybeP p = option Nothing $ fmap Just p

common :: Parser ([ByteString],
                  Maybe (ByteString,ByteString),
                  Address,
                  Maybe Integer)
common =
  do
    info <- many' informationPost
    names <- maybeP namePost
    (street,zipCode) <- fmap (maybe (Nothing,Nothing) $ \(a,b) -> (Just a,Just b)) $ maybeP addressPostOne
    (city,country,countryCode) <- fmap (maybe (Nothing,Nothing,Nothing) $ \(a,b,c) -> (Just a,Just b,Just c)) $ maybeP addressPostTwo
    orgNo <- maybeP orgNoPost
    return (info,names,Address street zipCode city country countryCode,orgNo)

payment :: Parser Entry
payment =
  do
    (bg,ref,amm,refc,payc,bgcno,im) <- paymentPost
    er <- many' (extraReferenceNumberPost bg 0 payc bgcno im <|>
                 extraReferenceNumberPostNegative bg 0 payc bgcno im)
    (info,names,addr,orgNo) <- common
    return $ Entry Payment (Sender bg names addr orgNo) (mkReference refc ref amm:er) amm payc bgcno im info

deduction :: Parser Entry
deduction =
  do
    (bg,ref,amm,refc,payc,bgcno,im,dedc) <- deductionPost
    er <- many' (extraReferenceNumberPost bg 0 payc bgcno im <|>
                 extraReferenceNumberPostNegative bg 0 payc bgcno im)
    (info,names,addr,orgNo) <- common
    return $ Entry (Deduction dedc) (Sender bg names addr orgNo) (mkReference refc ref amm:er) amm payc bgcno im info

paymentPost :: Parser (Integer,ByteString,Centi,Int,PaymentChannel,Integer,Image)
paymentPost =
  (do
     string "20"
     bgNo <- takeRead 10
     ref <- myTake 25
     amm <- fmap MkFixed $ takeRead 18
     refc <- takeRead 1
     payc <- fmap mkPaymentChannel $ takeRead 1
     bgcNo <- takeRead 12
     im <- fmap mkImage $ takeRead 1
     tillEol
     return (bgNo,ref,amm,refc,payc,bgcNo,im)) <?> "paymentPost"

deductionPost :: Parser (Integer,ByteString,Centi,Int,PaymentChannel,Integer,Image,DeductionCode)
deductionPost =
  (do
     string "21"
     bgNo <- takeRead 10
     ref <- myTake 25
     amm <- fmap MkFixed $ takeRead 18
     refc <- takeRead 1
     payc <- fmap mkPaymentChannel $ takeRead 1
     bgcNo <- takeRead 12
     im <- fmap mkImage $ takeRead 1
     dedc <- fmap mkDeductionCode $ takeRead 1
     tillEol
     return (bgNo,ref,amm,refc,payc,bgcNo,im,dedc)) <?> "deductionPost"

referenceCommon :: Bool -> Integer -> Centi -> PaymentChannel -> Integer -> Image -> Parser Reference
referenceCommon isNeg bgNo' amm' payc' bgcNo' im' =
  do
    bgNo <- takeRead 10
    ref <- takeT 25
    amm <- fmap MkFixed $ takeRead 18
    refc <- takeRead 1
    payc <- fmap mkPaymentChannel $ takeRead 1
    bgcNo <- takeRead 12
    im <- fmap mkImage $ takeRead 1
    tillEol
    assert (bgNo' == bgNo) "bgNo doesn't match"
    assert (payc' == payc) "payc doesn't match"
    assert (bgcNo' == bgcNo) "bgcNo doesn't match"
    assert (im' == im) "im doesn't match"
    return $ mkReference refc ref (if isNeg then 0 - amm else amm)

extraReferenceNumberPost :: Integer -> Centi -> PaymentChannel -> Integer -> Image -> Parser Reference
extraReferenceNumberPost bgNo amm payc bgcNo im =
  (do
     string "22"
     referenceCommon False bgNo amm payc bgcNo im) <?> "extraReferenceNumberPost"

extraReferenceNumberPostNegative :: Integer -> Centi -> PaymentChannel -> Integer -> Image -> Parser Reference
extraReferenceNumberPostNegative bgNo amm payc bgcNo im =
  (do
     string "23"
     referenceCommon True bgNo amm payc bgcNo im) <?> "extraReferenceNumberPostNegative"

informationPost :: Parser ByteString
informationPost =
  (do
     string "25"
     info <- takeT 50
     tillEol
     return info) <?> "informationPost"

namePost :: Parser (ByteString,ByteString)
namePost =
  (do
     string "26"
     name <- takeT  35
     extraName <- takeT 35
     tillEol
     return (name,extraName)) <?> "namePost"

addressPostOne :: Parser (ByteString,ByteString)
addressPostOne =
  (do
     string "27"
     street <- takeT 35
     zipCode <- takeT 9
     tillEol
     return (street,zipCode)) <?> "addressPostOne"

addressPostTwo :: Parser (ByteString,ByteString,ByteString)
addressPostTwo =
  (do
   string "28"
   city <- takeT 35
   country <- takeT 35
   countryCode <- takeT 2
   tillEol
   return (city,country,countryCode)) <?> "addressPostTwo"

orgNoPost :: Parser Integer
orgNoPost =
  (do
     string "29"
     orgNo <- takeRead 12
     tillEol
     return orgNo) <?> "orgNoPost"

