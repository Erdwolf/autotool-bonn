module Autolib.NFA.Equiv.Generate where

-- -- $Id$

import Autolib.NFA.Type

import Autolib.NFA.Det (det0)
import Autolib.NFA.Normalize

import Autolib.NFA.Equiv.Challenger
import Autolib.NFA.Equiv.Core

import Autolib.NFA.Some

import Autolib.Dot
import System

import Autolib.Util.Zufall
import Autolib.Util.Cache
import Autolib.Util.Seed
import Autolib.Util.Datei

import Data.Set
import Autolib.Size
import Autolib.ToDoc
import Autolib.Reporter

import Autolib.Inter.Types
import Autolib.Informed

data Conf = Conf { alphabet :: Set Char
		 , nfa_size :: Int
		 , max_dfa_size :: Int
		 }
     deriving ( Eq, Ord, Show, Read )

--------------------------------------------------------------------------

roll :: Conf
     -> Datei
    -> IO ( NFA Char Int )
roll conf dat = do
    let sigma = alphabet conf
    ( d, xsss ) <- repeat_until
            ( do a <- nontrivial sigma $ nfa_size conf
		 let d = normalize $ det0 a
		 return ( d, zerlege sigma d )
	    ) ( \ (d, xsss) -> 
		   length xsss > 2 -- wenigstens zwei schritte
		&& size d < max_dfa_size conf
	        && ( (1 <) $ length -- wenigstens zwei klassen
	           $ filter (>1)  -- mit wenigstens zwei elementen
		   $ map cardinality $ setToList $ last xsss 
	    ) )

    link <- dotty dat d

    return $ informed ( toDoc d ) d

------------------------------------------------------------------------

-- TODO: gehört in anderes file

dotty :: ( NFAC c s, Show c, Show s )
      => Datei
      -> NFA c s
      -> IO String
dotty d aut = do

    let 
        dotfile = name d ++ "." ++ "dot"
	pngfile = name d ++ "." ++ extension d
    let pic = toDot aut    

    when ( extension d `elem` [ "png" ] ) $ do
        dirgehen d
        writeFile dotfile $ show pic ++ "\n\n"
        system $ unwords [ "dot"
                     , "-Grankdir=LR", "-T" ++ extension d
                     , "-o", pngfile
                     , dotfile
                     ]
	return ()

    return $ "Get.cgi?" ++ outer d 

------------------------------------------------------------------------

this :: Conf -> Var Equiv (NFA Char Int) [[Trenner Char Int]]
this conf = x where x = Var  { problem = Equiv
      , aufgabe = "E"
      , version = "Quiz"
      , key	= \ matrikel -> return matrikel
      , gen	= \ key -> do
            seed $ read key
	    let dat = Datei { pfad = [ "autotool", "store"
				       , aufgabe x, version x, key
				      ]
			    , name = "automat"
			    , extension = "png"
			    }
	    i <- cache (  Datei { pfad = [ "autotool", "cache"
                                              , aufgabe x, version x
                                              ]
                                     , name = key
				     , extension = "cache"
                                     }
                            ) ( roll conf dat )
	    return $ return i
      }
	    



