{-# language TypeSynonymInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

-- | SQL formatted time (not duration)

module Control.Time.Typ where

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Xml

import Data.Typeable
import Data.Ix

data Time = Time
          { year :: Int, month :: Int, day :: Int
	  , hour :: Int, minute :: Int, second :: Int
	  }
     deriving ( Eq, Ord , Typeable )

instance ToDoc Time where
    toDoc t = hsep
        [ hcat $ punctuate ( text "-" ) 
	       $ map toDoc [ year t, month t, day t ]
	, hcat $ punctuate ( text ":" ) 
	       $ map toDoc [ hour t, minute t, second t ]
	]

instance Reader Time where
    reader = do
        let natural = do
                cs <- many digit
		return $ read cs
            get tag bnd = do
	        n <- natural
		if inRange bnd n
		   then return n
		   else unexpected ( tag ++ " nicht im Bereich " ++ show bnd )
        y <- get "Jahr" ( 2000, 2010 ) 
	Autolib.Reader.char '-'
	m <- get "Monat" ( 1, 12 )
	Autolib.Reader.char '-' 
	d <- get "Tag" ( 1, 31 )
	my_whiteSpace
	h <- get "Stunde" ( 0, 23 )
        Autolib.Reader.char ':'
	i <- get "Minute" ( 0, 59 )
        Autolib.Reader.char ':'
	s <- get "Sekunde" ( 0, 59 )
        my_whiteSpace
	return $ Time 
	    { year = y, month = m, day = d
	    , hour = h, minute = i, second = s
	    }

instance Container Time String where
    label t = "Time"
    pack    = show
    unpack  = read
