module Wash.Fields where

import Char
import Data.List

class Reason a where
  reason :: a -> String
  reason _ = ""

instance Reason Int where
  reason _ = "Int"

instance Reason Integer where
  reason _ = "Integer"

instance Reason Double where
  reason _ = "Double"

instance Reason Float where
  reason _ = "Float"

instance Reason Bool where
  reason _ = "Bool"

instance Reason () where
  reason _ = "()"

instance Reason Char where
  reason _ = "Char"

instance Reason a => Reason [a] where
  reason xs = "[" ++ reason (head xs) ++ "]"

instance (Reason a, Reason b) => Reason (a, b) where
  reason (x,y) = "(" ++ reason x ++ "," ++ reason y ++ ")"

-- 
newtype EmailAddress = EmailAddress { unEmailAddress :: String }

instance Read EmailAddress where
  readsPrec i str = 
    let isAddressChar c = isAlpha c || isDigit c || c `elem` ".-_"
        (name, atDomain) = span isAddressChar (dropWhile isSpace str)
    in  case atDomain of
          '@' : domainPart ->
	    let (domain, rest) = span isAddressChar domainPart in
	    let fulladdress = name ++ '@' : domain in
	    if null name || null domain 
	    || "." `isPrefixOf` name || "." `isSuffixOf` name || "." `isPrefixOf` domain
	    || any (isPrefixOf "..") (tails name) 
	    || any (isPrefixOf "..") (tails domain)
	    then []
	    else [(EmailAddress fulladdress, dropWhile isSpace rest)]
	  _ -> []
  readList str = 
    case reads str of
      (em1, str'): _ ->
        case str' of
	  ',': str'' ->
	    case readList str'' of
	      (ems, str'''): _ ->
	        [(em1:ems, str''')]
	      _ ->
	        [([em1], str')]
	  _ -> 
	    [([em1], str')]
      _ -> []
	

instance Show EmailAddress where
  showsPrec i (EmailAddress str) = showString str

instance Reason EmailAddress where
  reason _ = "email address {must contain @ and no special characters except . - _}"
 
-- 
newtype CreditCardNumber = CreditCardNumber { unCreditCardNumber :: String }

instance Read CreditCardNumber where
  readsPrec i str = 
    let str' = dropWhile isSpace str
	str'' =  take 16 str'
	str''' = dropWhile isSpace (drop 16 str'')
    in	if length str'' == 16 && all isDigit str && luhnCheck str
        then [(CreditCardNumber str, str''')]
	else []

luhnCheck str = 
  checkEven (reverse str) 0
  where digitval d = ord d - ord '0'
	checkEven [] n = n `mod` 10 == 0
	checkEven (d:ds) n = checkOdd ds (n + digitval d)
	checkOdd [] n = n `mod` 10 == 0
	checkOdd (d:ds) n = checkEven ds (n + doubleval (digitval d))
	doubleval d = let d2 = 2*d in if d2 > 9 then d2 - 9 else d2

instance Show CreditCardNumber where
  showsPrec i (CreditCardNumber str) = showString str

instance Reason CreditCardNumber where
  reason _ = "credit card number"
-- 
data CreditCardExp = CreditCardExp { cceMonth :: Int, cceYear :: Int }

instance Read CreditCardExp where -- ##/##
  readsPrec i str = 
    g $ dropWhile isSpace str
    where
    g str@(mh: ml: '/': yh: yl:cces) =
      let mo, yr :: Int
	  mo = read [mh, ml]
	  yr = read [yh, yl]
      in if isDigit mh && isDigit ml && isDigit yh && isDigit yl && mo >= 1 && mo <= 12
      then [(CreditCardExp mo yr, cces)]
      else []
    g _ = []

instance Show CreditCardExp where
  showsPrec i cce = shows (cceMonth cce) . showChar '/' . shows (cceYear cce)

instance Reason CreditCardExp where
  reason _ = "credit card expiration date: MM/YY"
-- 
newtype NonEmpty = NonEmpty { unNonEmpty :: String }

instance Read NonEmpty where
  readsPrec i [] = []
  readsPrec i str = [(NonEmpty str, "")]

instance Show NonEmpty where
  showsPrec i (NonEmpty str) = showString str

instance Reason NonEmpty where
  reason _ = "non empty string"
-- 
newtype AllDigits = AllDigits { unAllDigits :: String }

instance Read AllDigits where
  readsPrec i str = let str' = takeWhile isDigit str in 
			[(AllDigits str', dropWhile isDigit str)]

instance Show AllDigits where
  showsPrec i (AllDigits str) = showString str

instance Reason AllDigits where
  reason _ = "non empty string of digits"
-- 
newtype Text = Text { unText :: String }

instance Read Text where
  readsPrec i str = [(Text str, "")]
  
instance Show Text where
  showsPrec i (Text str) = showString str

instance Reason Text where
  reason _ = "arbitrary string"

{-- 
pfail str = []
psucc x str = [(x,str)]
pcond p (x:rest) | p x = [(x,rest)]
patom x = pcond (==x)
palt p1 p2 str = p1 str ++ p2 str
pseq p1 p2 str = [((x1,x2), s2) | (x1, s1) <- p1 str, (x2, s2) <- p2 s1]
pfun p f str = [(f x, rest) |  (x, rest) <- p str]
pstar p = palt (pfun (pseq p (pstar p)) (\ (p,ps) -> p:ps)) psucc []
--}
