module NFA.Equiv.Generate where

-- $Id$

import NFA.Type

import NFA.Det (det0)
import NFA.Normalize

import NFA.Equiv.Challenger
import NFA.Equiv.Core

import NFA.Some

import Dot
import System

import Util.Zufall
import Util.Cache
import Util.Seed
import Util.Datei

import Set
import Size
import ToDoc
import Reporter

import Inter.Types
import Informed

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
	    



