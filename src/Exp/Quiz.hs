module Autolib.Exp.Quiz where

-- -- $Id$

import qualified Autolib.Exp.Some
import Autolib.NFA.Type
import qualified Autolib.NFA.Synthese as S

import Autolib.Util.Seed
import Autolib.Util.Datei
import Autolib.Util.Cache

import Autolib.Inter.Types

import Autolib.Sets
import Autolib.ToDoc
import Autolib.Reporter

data Conf = Conf
         { alpha :: Set Char
         , exp_size :: Int
         , det :: Bool
         }
    deriving ( Eq, Ord, Read, Show )

make :: String -- aufgabe (major)
     -> String -- aufgabe (minor)
     -> Conf
     -> Var S.Synthese S.SI ( NFA Char Int )
make auf ver conf =
    Var { problem = S.Synthese
        , aufgabe = auf
        , version = ver
        , key = \ matrikel -> do 
          return matrikel
        , gen = \ key -> do
          seed $ read key
          ( i, b ) <- cache 
                 (  Datei { pfad = [ "autotool", "cache" , auf, ver ]
                          , name = key
			  , extension = "cache"
                          }
                 ) ( Exp.Some.nontrivial ( alpha conf ) ( exp_size conf ) )
          return $ do
	      inform $ vcat
	             [     text "Finden Sie einen"
                       <+> ( if det conf
			     then text "deterministischen" else empty )
	               <+> text "endlichen Automaten,"
		     , text "der die Sprache" <+> toDoc i
		     , text "über dem Alphabet" <+> toDoc ( alpha conf )
		     , text "akzeptiert."
		     ]
              return $ S.SI { S.name = ver
			  , S.ausdruck = i
			  , S.alphabet = alpha conf
			  , S.beschreibung = Nothing
			  , S.deterministisch = det conf
			  }

        }
