-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module PCProblem.Konfig where

import PCProblem.Type

import Autolib.ToDoc
import Autolib.Reader

data Konfig = 
     Konfig { instanz :: PCP
	    , folge :: Folge -- ^ achtung, falschrum! (damit wir vorn dranbauen)
	    , tief :: Int -- ^ im Suchbaum (= Länge der Folge)
	    , oben :: String  -- ^ einer von beiden soll leer sein
	    , unten :: String -- ^ d. h. gemeinsame präfixe werden abgeschnitten
	    }

-- | zum schnelleren vergleichen
wesen :: Konfig -> ( Int, Int, String, String )
wesen k = ( length $ oben k, length $ unten k
	  , oben k         , unten k 
	  )

instance Eq Konfig where 
    k1 == k2 = wesen k1 == wesen k2
instance Ord Konfig where 
    compare k1 k2 = compare ( wesen k1 ) ( wesen k2 )

$(derives [makeReader, makeToDoc] [''Konfig])

instance Show Konfig where show = render . toDoc
instance Read Konfig where readsPrec = parsec_readsPrec
