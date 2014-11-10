{- |
Module      :  $Header$
Description :  Datatypes for the BgMax file format
Copyright   :  (c) Petter Bergman
License     :  BSD3

Maintainer  :  jon.petter.bergman@gmail.com
Stability   :  experimental
Portability :  portable

This module contain data definitions for the different types of "Posts" or 
"Transaction Codes" that may occur in a BgMax file, as well as auxiliary
data types. 

The datatypes should map in a straightforward way to the 
documentation from <http://www.bgc.se/upload/Gemensamt/Trycksaker/Manualer/BG6040.pdf Bankgirocentralen>.
-}
module Finance.BgMax.TK where
import Data.Time(LocalTime(..),Day)
import Data.ByteString(ByteString)

-- | Testmark
data TestMark = 
    T -- ^ Test file  
  | P -- ^ Production file 
  deriving Show

-- | BankGirot currently only supports EUR and SEK
data Currency = EUR | SEK deriving Show

-- | Reference.
--   The references correspond to the reference codes defined in
--   <http://www.bgc.se/upload/Gemensamt/Trycksaker/Manualer/BG6040.pdf the BgMax documentation>.
data Reference = 
  -- | No reference.
    Ref0
  -- | No reference. 
  | Ref1 
  -- | Reference is correct OCR-no.
  | Ref2 Integer
  -- | One or more references.
  | Ref3 ByteString
  -- | One reference.
  | Ref4 ByteString
  -- | Incorrect reference.
  | Ref5 ByteString
  -- | Reserved for future use.
  | Ref Int ByteString 
  deriving Show

-- | Deposit type.
--   Deposit type according to bank agreement.
data DepositType = K | D | S deriving Show

-- | Transaction type reused in TK 20,21,22,23.
data Transaction = 
  Transaction {
    senderBG :: Integer,
    reference :: Reference,
    ammount :: Integer,
    -- | See the BGC documentation for the meaning of payment channel code.
    paymentChannel :: Int,
    bcgNo :: Integer,
    -- | See the BGC documentation for the meaning of the image mark.
    imageMark :: Int } 
    deriving Show

-- | Start post.
data TK01_t =
  TK01_t {
    layoutName :: ByteString,
    layoutVersion :: Int,
    timeStamp :: LocalTime,
    testMark :: TestMark } 
    deriving Show

-- | Opening post.
data TK05_t =
  TK05_t {
    recipientBG :: Integer,
    recipientPG :: Maybe Integer,
    currency :: Currency } 
    deriving Show

-- | Payment post.
data TK20_t =
  TK20_t {
    payment :: Transaction } 
    deriving Show

-- | Deduction post.
data TK21_t =
  TK21_t {
    deduction :: Transaction,
    -- | See the BGC documentation for the meaning of the deduction code.
    deductionCode :: Int } 
    deriving Show

-- | Extra reference post.
data TK22_t =
  TK22_t {
    positiveReference :: Transaction } 
    deriving Show

-- | Extra reference post (negative ammount).
data TK23_t =
  TK23_t {
    negativeReference :: Transaction } 
    deriving Show

-- | Information post.
data TK25_t =
  TK25_t {
    information :: ByteString } 
    deriving Show

-- | Name post.
data TK26_t =
  TK26_t {
    name :: ByteString,
    extraName :: ByteString } 
    deriving Show

-- | Address 1 post.
data TK27_t =
  TK27_t {
    address :: ByteString,
    zipCode :: ByteString } 
    deriving Show

-- | Address 2 post.
data TK28_t =
  TK28_t {
    city :: ByteString,
    country :: ByteString,
    countryCode :: ByteString } 
    deriving Show

-- | Organisation-number post.
data TK29_t = 
  TK29_t {
    orgNo :: Integer } 
    deriving Show

-- | Deposit post.
data TK15_t =
  TK15_t {
    recipientAccount :: Integer,
    paymentDate :: Day,
    depositNo :: Int,
    depositAmmount :: Integer,
    depositCurrency :: Currency,
    paymentDeductionCount :: Int,
    depositType :: Maybe DepositType } 
    deriving Show

-- | End post
data TK70_t =
  TK70_t {
    paymentsCount :: Int,
    deductionsCount :: Int,
    extraReferenceCount :: Int,
    depositCount :: Int } 
    deriving Show

-- | Sum type for posts.
data Post =
    TK01 TK01_t
  | TK05 TK05_t
  | TK20 TK20_t
  | TK21 TK21_t
  | TK22 TK22_t
  | TK23 TK23_t
  | TK25 TK25_t
  | TK26 TK26_t
  | TK27 TK27_t
  | TK28 TK28_t
  | TK29 TK29_t
  | TK15 TK15_t
  | TK70 TK70_t
  -- | The BGC documentation states that implementations should ignore posts 
  --   they don't understand, they will be instead be recorded using this 
  --   constructor.
  | TK Int 
  deriving Show
