module Grammatik.CF.Quiz where

--  $Id$

import Grammatik.Type
import Language.Type

import qualified Grammatik.CF.Generate as G
import qualified Grammatik.CF.Language as L
import qualified Grammatik.CF.Create
import Grammatik.CF.Interface

import qualified Grammatik.CF.Instance.Config as I

import qualified Reporter.Checker as C

import Inter.Types
import Util.Seed
import Util.Wort
import Util.Cache
import Util.Datei
import Util.Zufall
import Edit

import Data.Maybe ( isJust )
import Data.List ( partition )

data Config =
     Config { generate :: G.Config
	    , typ :: C.Type Grammatik
	    , num_samples :: Int
	    , min_sample_length :: Int
	    , max_sample_length :: Int
	    }

quiz :: Config -> Var CFG I.Config Grammatik
quiz conf = 
    let auf = "CFG"
	ver = "Quiz" ++ C.nametag (typ conf)
    in Var { problem = CFG
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> return matrikel
	, gen = \ key -> do
	  seed $ read key
	  g <- cache (  Datei { pfad = [ "autotool", "cache" , auf, ver ]
                                     , name = key
                                     , extension = "cache"
                                     }
                            ) ( throw conf )
	  c <- mach conf g
	  return $ return c
	}

throw :: Config -> IO Grammatik
throw conf = do
    Just g <- repeat_until ( G.roll $ generate conf ) 
			   ( \ mg -> isJust mg ) 
    return g

mach :: Config -> Grammatik -> IO I.Config 
mach conf g = do
    let l = L.make "Quiz" g
    let origs = take ( num_samples conf ) 
	      $  ( Grammatik.CF.Create.create g 
		 $ max_sample_length conf 
		 ) 
    mutants <- edits origs
    let cands = alles (setToList $ terminale g) 5 
	      ++ origs ++ mutants
    let ( yeah, noh ) = partition ( contains l ) cands
    return $ I.Config 
	   { I.lang = l
	   , I.typ = typ conf
	   , I.yeah = take ( num_samples conf ) yeah
	   , I.noh  = take ( num_samples conf ) noh
	   }

